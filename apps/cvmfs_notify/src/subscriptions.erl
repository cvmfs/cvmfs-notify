%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc subcriptions manages the subscriptions for different repos
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(subscriptions).

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/0, subscribe/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% Records
-record(repo_monitor, {uid, pid}).
-record(repo_state, {name, msg, subscriptions}).


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

    ets:new(subs, [set, private, named_table]),
    ets:new(monitors, [set, private, named_table]),

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
    Reply = p_subscribe(Pid, Repo),
    lager:debug("Subscribe event: pid: ~p, repo: ~p - reply: ~p",
               [Pid, Repo, Reply]),
    {reply, Reply, State}.


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
handle_info({'DOWN', Ref, process, Pid, _}, State) ->
    lager:debug("Monitored process is down: ref: ~p, pid: ~p", [Ref, Pid]),

    ets:delete(monitors, Ref),

    MS = ets:fun2ms(fun(RepoState) -> p_remove_pid(Pid, RepoState) end),
    ets:select_replace(subs, MS),

    {noreply, State};
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

p_subscribe(Pid, Repo) ->
    % Retrieve the subscription information for Repo, or insert a new item
    RepoState = case ets:lookup(subs, Repo) of
                    [RS | _] ->
                        RS;
                    _ ->
                        #repo_state{name = Repo,
                                    msg = empty,
                                    subscriptions = sets:new()}
                end,

    Subscriptions = RepoState#repo_state.subscriptions,

    % Send a message to the subscriber if a message has already been
    % received for the repository
    case RepoState#repo_state.msg of
        empty ->
            false;
        Msg ->
            Pid ! {activity, Msg}
    end,

    % Register the new subcription for the repository
    Ref = erlang:monitor(process, Pid),
    NewSubscriptions = set:add_element(Pid, Subscriptions),
    NewRepoState = RepoState#repo_state{subscriptions = NewSubscriptions},
    ets:insert(subs, NewRepoState),
    ets:insert(monitors, #repo_monitor{uid = Ref, pid = Pid}),
    ok.


p_remove_pid(Pid, RepoState) ->
    NewSubscriptions = sets:del_element(Pid,
                                        RepoState#repo_state.subscriptions),
    RepoState#repo_state{subscriptions = NewSubscriptions}.
