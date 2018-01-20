%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc _handler - trigger request handler
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(event_manager).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/0, subscribe/3, trigger/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% Records

-record(repo_state, {revision, root_hash, subscriptions}).
-record(saved_repo_state, {revision, root_hash}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% Adds a subscription to notification about Repo

%% @end
%%--------------------------------------------------------------------
subscribe(Id, Repo, MinRevision) ->
    gen_server:call(?MODULE, {subscribe, {Id, Repo, MinRevision}}).


%%--------------------------------------------------------------------
%% @doc
%% Trigger a notification about a new revision in Repo
%%
%% @end
%%--------------------------------------------------------------------
trigger(Repo, Revision, RootHash) ->
    gen_server:call(?MODULE, {trigger, {Repo, Revision, RootHash}}).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    State = p_read_initial_state(),
    lager:info("Event manager started"),
    {ok, {State, #{}}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({subscribe, {Pid, Repo, MinRevision}}, _From, State) ->
    {Reply, NewState} = p_subscribe(Pid, Repo, MinRevision, State),
    lager:info("Subscribe event: pid: ~p, repo: ~p, min_revision: ~p - reply: ~p",
               [Pid, Repo, MinRevision, Reply]),
    {reply, Reply, NewState};
handle_call({trigger, {Repo, Revision, RootHash}}, _From, State) ->
    {Reply, NewState} = p_trigger(Repo, Revision, RootHash, State),
    lager:info("Trigger event: repo: ~p, revision: ~p, root_hash: ~p - reply: ~p",
               [Repo, Revision, RootHash, Reply]),
    {reply, Reply, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    lager:info("Cast received: ~p -> noreply", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', Ref, process, Pid, _}, {State, Monitors}) ->
    lager:info("Monitored process is down: ref: ~p, pid: ~p", [Ref, Pid]),
    NewMonitors = maps:remove(Ref, Monitors),
    NewState = maps:map(fun(_, RepoState) ->
                                p_remove_pid(Pid, RepoState)
                        end,
                        State),
    {noreply, {NewState, NewMonitors}};
handle_info(Info, State) ->
    lager:warning("Unknown message received: ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    lager:info("Terminating with reason: ~p", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(OldVsn, State, _Extra) ->
    lager:info("Code change request received. Old version: ~p", [OldVsn]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

p_subscribe(Pid, Repo, MinRevision, {State, Monitors}) ->
    % Retrieve the subscription information for Repo, or insert a new item
    RepoState = maps:get(Repo, State, #repo_state{revision = unknown,
                                                  root_hash = unknown,
                                                  subscriptions = #{}}),

    Subscriptions = RepoState#repo_state.subscriptions,

    % Retrieve the current revision of the repo
    case RepoState#repo_state.revision of
        unknown ->
            false;
        Revision when Revision >= MinRevision ->
            % If the current revision is already >= MinRevision, notify Id
            Pid ! {repo_updated, Revision, RepoState#repo_state.root_hash};
        _ ->
            false
    end,

    Ref = erlang:monitor(process, Pid),
    NewSubscriptions = maps:put(Pid, MinRevision, Subscriptions),
    NewRepoState = RepoState#repo_state{subscriptions = NewSubscriptions},
    NewState = maps:put(Repo, NewRepoState, State),
    NewMonitors = Monitors#{ Ref => Pid },
    {ok, {NewState, NewMonitors}}.

p_trigger(Repo, Revision, RootHash, {State, Monitors}) ->
    % Retrieve the subscription information for Repo, or insert a new item
    RepoState = maps:get(Repo, State, #repo_state{revision = unknown,
                                                  root_hash = unknown,
                                                  subscriptions = #{}}),

    % Inspect subscriptions and notify if needed
    Subscriptions = RepoState#repo_state.subscriptions,
    Notify = fun(Pid, MinRevision) ->
                     case Revision >= MinRevision of
                         % A larger revision than MinRevision has arrived. Notify the subscriber
                         true ->
                             Pid ! {repo_updated, Revision, RootHash},
                             Revision + 1;
                         _ ->
                             MinRevision
                     end
             end,
    NewSubscriptions = maps:map(Notify, Subscriptions),

    NewRepoState = RepoState#repo_state{revision = Revision,
                                        root_hash = RootHash,
                                        subscriptions = NewSubscriptions},
    NewState = maps:put(Repo, NewRepoState, State),
    p_save_repo_state(Repo, NewRepoState),

    {ok, {NewState, Monitors}}.

p_remove_pid(Pid, RepoState) ->
    NewSubscriptions = maps:filter(
                         fun(P, _) ->
                                 P =/= Pid
                         end,
                         RepoState#repo_state.subscriptions),
    RepoState#repo_state{subscriptions = NewSubscriptions}.


p_read_initial_state() ->
    case application:get_env(db_dir) of
        {ok, DbDir} ->
            lager:info("DB dir: ~p", [DbDir]),
            dets:open_file(repo_state, [{file, DbDir ++ "/repo_state"}]),
            L = dets:traverse(repo_state,
                              fun({Name, State}) ->
                                      RS = #repo_state{revision = State#saved_repo_state.revision,
                                                       root_hash = State#saved_repo_state.root_hash,
                                                       subscriptions = #{}},
                                      {continue, {Name, RS}}
                              end),
            maps:from_list(L);
        _ ->
            #{}
    end.

p_save_repo_state(RepoName, RepoState) ->
    case application:get_env(db_dir) of
        {ok, _} ->
            S = #saved_repo_state{revision = RepoState#repo_state.revision,
                                  root_hash = RepoState#repo_state.root_hash},
            ok = dets:insert(repo_state, [{RepoName, S}]);
        _ ->
            false
    end.
