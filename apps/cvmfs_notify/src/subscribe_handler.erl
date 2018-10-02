%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc subsribe_handler - HTTP request handler for /subscribe
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(subscribe_handler).

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
    {URI, T0} = util:req_tick(Uid, Req0, micro_seconds),

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

    util:req_tock(Uid, URI, T0, micro_seconds),
    Reply.


%% Websocket callbacks

websocket_init(State) ->
    lager:debug("Websocket upgrade successful - State: ~p", [State]),
    {ok, #{}, hibernate}.

websocket_handle({binary, Msg}, State) ->
    lager:debug("Frame received from client: ~p, state: ~p", [Msg, State]),
    case jsx:is_json(Msg) of
        true ->
            case jsx:decode(Msg, [return_maps]) of
                #{<<"version">> := _Version,
                  <<"repository">> := Repo} ->
                    lager:debug("Subscription request: repo: ~p, pid: ~p", [Repo, self()]),
                    case subscriptions:subscribe(self(), Repo) of
                        ok ->
                            {ok, State, hibernate};
                        {error, Reason} ->
                            {reply, {binary,
                                     jsx:encode(#{<<"status">> => <<"error">>,
                                                  <<"reason">> => Reason})},
                             State, hibernate}
                    end;
                _ ->
                    {reply, {binary,
                             jsx:encode(#{<<"status">> => <<"error">>,
                                          <<"reason">> => <<"invalid subscription message">>})},
                     State, hibernate}
            end;
        false ->
            {reply, {binary,
                     jsx:encode(#{<<"status">> => <<"error">>,
                                  <<"reason">> => <<"message is not valid JSON">>})},
             State, hibernate}
    end;
websocket_handle({ping, _Msg}, State) ->
    lager:debug("Ping received from: ~p", [self()]),
    {ok, State, hibernate}.

websocket_info({activity, Msg}, State) ->
    lager:debug("Message received from backend: ~p, state: ~p", [Msg, State]),
    {reply, {binary, Msg}, State, hibernate};
websocket_info(Info, State) ->
    lager:notice("Unknown message received: ~p, state: ~p", [Info, State]),
    {ok, State, hibernate}.

