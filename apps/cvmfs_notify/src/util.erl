%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc utility functions
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(util).

-compile([{parse_transform, lager_transform}]).

-export([tick/3
        ,tock/4
        ,unique_id/0
        ,read_vars/2
        ,read_config_file/1
        ,default_config/0]).

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

read_vars(VarName, Defaults) ->
    case application:get_env(VarName) of
        {ok, {file, ConfigFile}} ->
            read_config_file(ConfigFile);
        {ok, ConfigMap} ->
            ConfigMap;
        undefined ->
            Defaults
    end.

read_config_file(File) ->
    case file:read_file(File) of
        {ok, Data} ->
            jsx:decode(Data, [{labels, atom}, return_maps]);
        {error, Reason} ->
            {error, Reason}
    end.

default_config() ->
    #{port => 4930,
      rmq_url => "http://localhost:5672"}.
