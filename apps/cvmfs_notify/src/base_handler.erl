%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc base_handler - base HTTP request handler
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(base_handler).

-compile([{parse_transform, lager_transform}]).

-export([init/2,
         websocket_init/1, websocket_handle/2, websocket_info/2]).

%%--------------------------------------------------------------------
%% @doc
%% Handles requests for the /notify end point
%%
%% If the message signature checks out, the connection is updated to
%% Websockets. Any future activity for the connection will be handled
%% by one of the websocket_*** callbacks.
%%
%% @end
%%--------------------------------------------------------------------
init(Req0, State) ->
    Uid = util:unique_id(),
    {URI, T0} = util:tick(Uid, Req0, micro_seconds),

    Reply = case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req0) of
                undefined ->
                    {cowboy_websocket, Req0, State};
                Subprotocols ->
                    case lists:keymember(<<"json">>, 1, Subprotocols) of
                        true ->
                            {cowboy_websocket,
                             cowboy_req:set_resp_header(<<"sec-websocket-protocol">>,
                                                        <<"json">>, Req0),
                             State};
                        false ->
                            {ok, cowboy_req:reply(400, Req0), State}
                    end
            end,

    util:tock(Uid, URI, T0, micro_seconds),
    Reply.


%% Websocket callbacks

websocket_init(State) ->
    lager:info("Websocket upgrade successful - State: ~p", [State]),
    {ok, State, hibernate}.

websocket_handle(Frame, State) ->
    lager:info("Frame received: ~p, state: ~p", [Frame, State]),
    {reply, {text, <<"Hello, client!">>}, State, hibernate}.
    %%{ok, State, hibernate}.

websocket_info(Info, State) ->
    lager:info("Erlang message received: ~p, state: ~p", [Info, State]),
    {ok, State, hibernate}.
