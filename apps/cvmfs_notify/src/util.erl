%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc utility functions
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(util).

-compile([{parse_transform, lager_transform}]).

-export([tick/3, tock/4, unique_id/0]).

unique_id() ->
    base64:encode(uuid:get_v4_urandom()).


tick(Uid, Req, Unit) ->
    T = erlang:monotonic_time(Unit),
    URI = cowboy_req:uri(Req),
    lager:debug("HTTP req received - Uid: ~p, URI: ~p", [Uid, URI]),
    {URI, T}.


tock(Uid, URI, T0, Unit) ->
    T1 = erlang:monotonic_time(Unit),
    lager:debug("HTTP req handled - Uid: ~p, URI: ~p, time to process = ~p usec",
               [Uid, URI, T1 - T0]).

