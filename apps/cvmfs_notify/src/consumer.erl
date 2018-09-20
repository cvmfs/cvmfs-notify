%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc AMQP consumer
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(consumer).

-include_lib("amqp_client/include/amqp_client.hrl").

-behaviour(gen_server).

-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/2]).

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
-spec start_link(Credentials, Repo) -> {ok, Pid} | ignore | {error, Error}
                              when Credentials :: #{ atom() => term() },
                                   Repo :: binary(),
                                   Pid :: pid(),
                                   Error :: {already_start, pid()} | term().
start_link(Credentials, Repo) ->
    gen_server:start_link(?MODULE, [Credentials, Repo], []).


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
init([Credentials, Repo]) ->
    Params = #amqp_params_network {
        username = maps:get(user, Credentials),
        password = maps:get(pass, Credentials),
        virtual_host = <<"/cvmfs">>,
        host = binary_to_list(maps:get(url, Credentials)),
        port = maps:get(port, Credentials),
        channel_max = 2047,
        frame_max = 0,
        heartbeat = 30
    },
    {ok, Connection} = amqp_connection:start(Params),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    lager:info("Consumer started for repository: ~p", [Repo]),

    {ok, #{connection => Connection,
           channel => Channel,
           exchange => maps:get(exchange, Credentials)}}.

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
handle_call(Msg, _From, State) ->
    lager:info("Call received: ~p", [Msg]),
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

