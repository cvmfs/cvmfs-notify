%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc _handler - event_manager
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(subscriptions).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/0, subscribe/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% Records
-record(repo_state, {revision, root_hash, subscriptions}).


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
    lager:info("Subscription manager started"),
    {ok, #{}}.


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
    lager:debug("Subscribe event: pid: ~p, repo: ~p, min_revision: ~p - reply: ~p",
               [Pid, Repo, MinRevision, Reply]),
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
    lager:debug("Cast received: ~p -> noreply", [Msg]),
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
    lager:debug("Monitored process is down: ref: ~p, pid: ~p", [Ref, Pid]),
    NewMonitors = maps:remove(Ref, Monitors),
    NewState = maps:map(fun(_, RepoState) ->
                                p_remove_pid(Pid, RepoState)
                        end,
                        State),
    {noreply, {NewState, NewMonitors}};
handle_info(Info, State) ->
    lager:info("Unknown message received: ~p", [Info]),
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


p_remove_pid(Pid, RepoState) ->
    NewSubscriptions = maps:filter(
                         fun(P, _) ->
                                 P =/= Pid
                         end,
                         RepoState#repo_state.subscriptions),
    RepoState#repo_state{subscriptions = NewSubscriptions}.
