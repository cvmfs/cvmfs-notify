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
        ,read_vars/0]).



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


read_vars() ->
    DefaultVars = default_config(),
    FileVars = maps:merge(DefaultVars, read_vars(user_config)),

    AMQPUser = os:getenv("CVMFS_NOTIFY_AMQP_USERNAME"),
    AMQPPass = os:getenv("CVMFS_NOTIFY_AMQP_PASSWORD"),

    EnvVars = case lists:all(
        fun(V) -> (V =/= false) and (V =/= []) end,
        [AMQPUser, AMQPPass]) of
        true ->
            #{rabbitmq_user => AMQPUser, rabbitmq_pass => AMQPPass};
        _ ->
            #{}
    end,
    maps:merge(FileVars, EnvVars).


%%===============================
%% Private functions
%%===============================

read_vars(VarName) ->
    case application:get_env(VarName) of
        {ok, {file, ConfigFile}} ->
            read_config_file(ConfigFile);
        {ok, ConfigMap} ->
            ConfigMap;
        undefined ->
            #{}
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
      rabbitmq_url => "http://localhost:5672",
      rabbitmq_user => not_given,
      rabbitmq_pass => not_given}.
