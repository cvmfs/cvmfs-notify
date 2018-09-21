%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc Consumer manager keeps track of consumers
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(consumer_mgr).

-include_lib("amqp_client/include/amqp_client.hrl").

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/0, ensure_started/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


%%===================================================================
%% API
%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid} | ignore | {error, Error}
                            when Pid :: pid(),
                                 Error :: {already_start, pid()} | term().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% Start a new AMQP consumer for the given repository name
%%
%% @spec ensure_started(Repo) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec ensure_started(Repo) -> {ok, Pid} | ignore | {error, Error}
                              when Repo :: binary(),
                                   Pid :: pid(),
                                   Error :: {already_start, pid()} | term().
ensure_started(Repo) ->
    gen_server:call(?MODULE, {ensure_started, Repo}).


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
    lager:info("Consumer manager started."),
    {ok, gb_sets:new()}.


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
handle_call({ensure_started, Repo}, _From, Consumers) ->
    NewConsumers = case gb_sets:is_element(Repo, Consumers) of
        false ->
            consumer_sup:start_consumer(Repo),
            gb_sets:add_element(Repo, Consumers);
        true ->
            Consumers
    end,
    {reply, {}, NewConsumers}.


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
handle_info(Msg, State) ->
    lager:notice("Unknown message received: ~p", [Msg]),
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
    lager:notice("Code change request received. Old version: ~p", [OldVsn]),
    {ok, State}.
