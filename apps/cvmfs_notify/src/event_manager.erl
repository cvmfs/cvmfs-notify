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
-export([start_link/0, subscribe/3, unsubscribe/2, trigger/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

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
%% Adds a subscription to notification about Repo

%% @end
%%--------------------------------------------------------------------
unsubscribe(Id, Repo) ->
    gen_server:call(?MODULE, {unsubscribe, {Id, Repo}}).


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
    lager:info("Event manager started"),
    {ok, {#{}, #{}}}.

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
handle_call({unsubscribe, {Pid, Repo}}, _From, State) ->
    {Reply, NewState} = p_unsubscribe(Pid, Repo, State),
    lager:info("Unsubscribe event: pid: ~p, repo: ~p - reply: ~p",
               [Pid, Repo, Reply]),
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
                                p_remove_ref(Ref, RepoState)
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
    RepoState = maps:get(Repo, State, maps:new()),

    Subscriptions = maps:get(subscriptions, RepoState, maps:new()),

    % Retrieve the current revision of the repo
    case maps:is_key(revision, RepoState) of
        true ->
            Revision = maps:get(revision, RepoState),
            case Revision >= MinRevision of
                true ->
                    % If the current revision is already >= MinRevision, notify Id
                    Pid ! {repo_updated, Revision, maps:get(root_hash, RepoState)};
                _ ->
                    false
            end;
        _ ->
            false
    end,
    Ref = erlang:monitor(process, Pid),
    NewSubscriptions = maps:put(Pid, {MinRevision, Ref}, Subscriptions),
    NewRepoState = maps:put(subscriptions, NewSubscriptions, RepoState),
    NewState = maps:put(Repo, NewRepoState, State),
    NewMonitors = Monitors#{ Ref => Pid },
    {ok, {NewState, NewMonitors}}.

p_unsubscribe(Pid, Repo, {State, Monitors}) ->
    % Retrieve the subscription information for Repo
    RepoState = maps:get(Repo, State),
    Subscriptions = maps:get(subscriptions, RepoState, maps:new()),
    {NewSubscriptions, NewMonitors} = case maps:is_key(Pid, Subscriptions) of
                                          true ->
                                              #{Pid := {_, Ref}} = Subscriptions,
                                              {maps:remove(Pid, Subscriptions),
                                               erlang:demonitor(Ref, [flush]),
                                               maps:remove(Ref, Monitors)};
                                          _ ->
                                              {Subscriptions, Monitors}
                       end,
    NewRepoState = maps:put(subscriptions, NewSubscriptions, RepoState),
    NewState = maps:put(Repo, NewRepoState, State),
    {ok, {NewState, NewMonitors}}.

p_trigger(Repo, Revision, RootHash, {State, Monitors}) ->
    % Retrieve the subscription information for Repo, or insert a new item
    RepoState = maps:get(Repo, State, maps:new()),

    % Inspect subscriptions and notify if needed
    Subscriptions = maps:get(subscriptions, RepoState, maps:new()),
    Notify = fun(Pid, {MinRevision, Ref}) ->
                     case Revision >= MinRevision of
                         % A larger revision than MinRevision has arrived. Notify the subscriber
                         true ->
                             Pid ! {repo_updated, Revision, RootHash},
                             {Revision + 1, Ref};
                         _ ->
                             {MinRevision, Ref}
                     end
             end,
    NewSubscriptions = maps:map(Notify, Subscriptions),

    NewRepoState = RepoState#{revision => Revision,
                              root_hash => RootHash,
                              subscriptions => NewSubscriptions},
    NewState = maps:put(Repo, NewRepoState, State),

    {ok, {NewState, Monitors}}.

p_remove_ref(Ref, RepoState) ->
    NewSubscriptions = maps:filter(
                         fun(_, {_, R}) ->
                                 R =/= Ref
                         end,
                         maps:get(subscriptions, RepoState, maps:new())),
    maps:put(subscriptions, NewSubscriptions, RepoState).

