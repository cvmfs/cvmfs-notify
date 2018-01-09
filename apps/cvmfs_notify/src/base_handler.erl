%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc base_handler - base HTTP request handler
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(base_handler).

-compile([{parse_transform, lager_transform}]).

-export([init/2]).

%%--------------------------------------------------------------------
%% @doc
%% Handles requests for the /api resource
%%
%% Return a list of available resources.
%%
%% @end
%%--------------------------------------------------------------------
init(Req0, State) ->
    Uid = util:unique_id(),
    {URI, T0} = util:tick(Uid, Req0, micro_seconds),

    Banner = <<"You are in an open field on the west side of a white house with a boarded front door.">>,
    API = #{<<"banner">> => Banner,
            <<"resources">> => []},
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/plain">>},
                           jsx:encode(API),
                           Req0),

    util:tock(Uid, URI, T0, micro_seconds),
    {ok, Req, State}.

