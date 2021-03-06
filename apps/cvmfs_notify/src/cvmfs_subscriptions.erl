%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc subcriptions manages the subscriptions for different repos
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(cvmfs_subscriptions).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/0, subscribe/2, notify/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% Records
-record(mon, {uid, repo}).
-record(repo, {name, msg, subscriptions}).


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
subscribe(Id, Repo) ->
    gen_server:call(?MODULE, {subscribe, {Id, Repo}}).


%%--------------------------------------------------------------------
%% @doc
%% Send new message to all subcribers to a repository

%% @end
%%--------------------------------------------------------------------
notify(Repo, Msg) ->
    gen_server:call(?MODULE, {notify, {Repo, Msg}}).


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

    ets:new(repos, [set, protected, named_table, {keypos, 2}]),
    ets:new(monitors, [set, protected, named_table, {keypos, 2}]),

    lager:info("Subscription manager started"),
    {ok, {}}.


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
handle_call({subscribe, {Pid, Repo}}, _From, State) ->
    {reply, p_subscribe(Pid, Repo), State};
handle_call({notify, {Repo, Msg}}, _From, State) ->
    {reply, p_notify(Repo, Msg), State}.



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
    lager:notice("Cast received: ~p -> noreply", [Msg]),
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
handle_info({'DOWN', Ref, process, Pid, _}, State) ->
    lager:debug("Monitored process is down: ref: ~p, pid: ~p", [Ref, Pid]),

    [#mon{repo = Repo} | _] = ets:lookup(monitors, Ref),
    ets:delete(monitors, Ref),

    [RS | _] = ets:lookup(repos, Repo),
    NewRS = p_remove_pid(Pid, RS),
    ets:insert(repos, NewRS),

    {noreply, State};
handle_info(Info, State) ->
    lager:notice("Unknown message received: ~p", [Info]),
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

p_subscribe(Pid, Repo) ->
    % Retrieve the subscription information for Repo, or insert a new item
    RepoState = case ets:lookup(repos, Repo) of
                    [] ->
                        p_new_repo_state(Repo, empty);
                    [RS | _] ->
                        RS
                end,

    Subscriptions = RepoState#repo.subscriptions,

    % Send a message to the subscriber if a message has already been
    % received for the repository
    case RepoState#repo.msg of
        empty ->
            false;
        Msg ->
            Pid ! {activity, Msg}
    end,

    % Register the new subscription for the repository
    Ref = erlang:monitor(process, Pid),
    NewSubscriptions = gb_sets:add_element(Pid, Subscriptions),
    NewRepoState = RepoState#repo{subscriptions = NewSubscriptions},

    ets:insert(repos, NewRepoState),
    ets:insert(monitors, #mon{uid = Ref, repo = Repo}),

    % Ensure that a backend message consumer is started for the repository
    cvmfs_consumer_mgr:ensure_started(Repo),

    ok.


p_notify(Repo, Msg) ->
    RepoState = case ets:lookup(repos, Repo) of
                    [RS | _] ->
                        RS;
                    [] ->
                        RS = p_new_repo_state(Repo, Msg),
                        ets:insert(repos, RS),
                        RS
                end,
    Subscriptions = RepoState#repo.subscriptions,
    gb_sets:fold(fun(Pid, _) -> Pid ! {activity, Msg} end, [], Subscriptions),
    NewRepoState = RepoState#repo{msg = Msg},
    ets:insert(repos, NewRepoState),
    ok.


p_remove_pid(Pid, RepoState) ->
    NewSubscriptions = gb_sets:del_element(Pid, RepoState#repo.subscriptions),
    RepoState#repo{subscriptions = NewSubscriptions}.


p_new_repo_state(Repo, Msg) ->
    #repo{name = Repo, msg = Msg, subscriptions = gb_sets:new()}.

