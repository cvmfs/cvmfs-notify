%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc AMQP publisher
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(publisher).

-include_lib("amqp_client/include/amqp_client.hrl").

-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

%% API
-export([start_link/1, send/2]).

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
-spec start_link(Args) -> {ok, Pid} | ignore | {error, Error}
                              when Args :: term(), Pid :: pid(),
                                   Error :: {already_start, pid()} | term().
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).


%%--------------------------------------------------------------------
%% @doc
%% Publish a message related to a repository
%%
%% @spec send(Repo, Msg) -> {ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
-spec send(Repo:: binary(), Msg :: binary()) -> ok.
send(Repo, Msg) ->
    gen_server:call(?MODULE, {send_msg, Repo, Msg}).

%%===================================================================
%% gen_server callbacks
%%===================================================================

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
init(Args) ->
    Params = #amqp_params_network {
        username = maps:get(user, Args),
        password = maps:get(pass, Args),
        virtual_host = <<"/cvmfs">>,
        host = binary_to_list(maps:get(url, Args)),
        port = maps:get(port, Args),
        channel_max = 2047,
        frame_max = 0,
        heartbeat = 30
    },
    {ok, Connection} = amqp_connection:start(Params),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    lager:info("Publisher started"),
    {ok, #{connection => Connection,
           channel => Channel,
           exchange => maps:get(exchange, Args)}}.

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
handle_call({send_msg, Repo, Msg}, _From, #{channel := Channel,
                                            exchange := Exch} = State) ->
    Publish = #'basic.publish'{exchange = Exch,
                               routing_key = Repo},
    amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Msg}),
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, {}, State}.

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

