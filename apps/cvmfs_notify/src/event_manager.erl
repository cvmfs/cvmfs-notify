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
handle_call({subscribe, {Id, Repo, MinRevision}}, _From, State) ->
    {Reply, NewState} = p_subscribe(Id, Repo, MinRevision, State),
    lager:info("Subscribe event: id: ~p, repo: ~p, min_revision: ~p - reply: ~p",
               [Id, Repo, MinRevision, Reply]),
    {reply, Reply, NewState};
handle_call({unsubscribe, {Id, Repo}}, _From, State) ->
    {Reply, NewState} = p_unsubscribe(Id, Repo, State),
    lager:info("Unsubscribe event: id: ~p, repo: ~p - reply: ~p",
               [Id, Repo, Reply]),
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

p_subscribe(Id, Repo, MinRevision, State) ->
    % Retrieve the subscription information for Repo, or insert a new item
    RepoState = maps:get(Repo, State, maps:new()),

    Subscriptions = maps:get(subscriptions, RepoState, maps:new()),

    % Retrieve the current revision of the repo
    NewRepoState = case maps:is_key(revision, RepoState) of
                   false ->
                       % Insert Id to the list of subscribers for Repo
                       NewSubscriptions = maps:put(Id, MinRevision, Subscriptions),

                       % Update and return new state
                       maps:update(subscriptions, NewSubscriptions, RepoState);
                   true ->
                           Revision = maps:get(revision, RepoState),
                           case Revision >= MinRevision of
                               % If the current revision is already >= MinRevision, notify Id
                               % and skip adding it to the list of subscribers
                               true ->
                                   {Pid, Ref} = Id,
                                   Pid ! {Ref, {repo_updated, Revision, maps:get(root_hash, RepoState)}},
                                   RepoState;
                               % If the current revision is < MinRevision, add Id to the list
                               % of subscribers
                               false ->
                                   NewSubscriptions = maps:put(Id, MinRevision, Subscriptions),
                                   maps:update(subscriptions, NewSubscriptions, RepoState)
                           end
               end,

    {ok, maps:update(Repo, NewRepoState, State)}.

p_unsubscribe(Id, Repo, State) ->
    % Retrieve the subscription information for Repo
    RepoState = maps:get(Repo, State),
    Subscriptions = maps:get(subscriptions, RepoState, maps:new()),
    NewSubscriptions = case maps:is_key(Id, Subscriptions) of
                           true ->
                               maps:remove(Id, Subscriptions);
                           false ->
                               Subscriptions
                       end,
    NewRepoState = maps:update(subscriptions, NewSubscriptions, RepoState),

    {ok, maps:update(Repo, NewRepoState, State)}.

p_trigger(Repo, Revision, RootHash, State) ->
    % Retrieve the subscription information for Repo, or insert a new item
    RepoState = maps:get(Repo, State, maps:new()),

    % Inspect subscriptions, notify and remove if needed
    Subscriptions = maps:get(subscriptions, RepoState, maps:new()),
    Filter = fun(Id, MinRevision) ->
                     case Revision >= MinRevision of
                         % A larger revision than MinRevision has arrived. Notify the subscriber
                         % and remove it from the list
                         true ->
                             {Pid, Ref} = Id,
                             Pid ! {Ref, {repo_updated, Revision, RootHash}},
                             false;
                         % The new revision is still lower than the MinRevision asked by the
                         % subscriber; Leave the subscriber in the list
                         false ->
                             true
                     end
             end,

    NewSubscriptions = maps:filter(Filter, Subscriptions),

    NewRepoState = RepoState#{revision => Revision,
                              root_hash => RootHash,
                              subscriptions => Subscriptions},

    {ok, maps:update(Repo, NewRepoState, State)}.
