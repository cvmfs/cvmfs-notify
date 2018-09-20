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
        ,error_map/1
        ,unique_id/0
        ,read_vars/0
        ,set_lager_log_level/1]).



-spec error_map(Reason :: binary()) -> #{ binary() => binary() }.
error_map(Reason) ->
    #{<<"status">> => <<"error">>, <<"reason">> => Reason}.


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
    RmqDefaultVars = maps:get(rabbitmq, DefaultVars),

    FileVars0 = maps:merge(DefaultVars, read_vars(user_config)),
    RmqFileVars = maps:get(rabbitmq, FileVars0, #{}),
    FileVars = maps:put(rabbitmq,
                        maps:merge(RmqDefaultVars, RmqFileVars),
                        FileVars0),

    AMQPUser = os:getenv("CVMFS_NOTIFY_AMQP_USERNAME"),
    AMQPPass = os:getenv("CVMFS_NOTIFY_AMQP_PASSWORD"),

    RmqVars = maps:get(rabbitmq, FileVars),

    EnvVars = case lists:all(
        fun(V) -> (V =/= false) and (V =/= []) end,
        [AMQPUser, AMQPPass]) of
        true ->
            #{user => list_to_binary(AMQPUser),
              pass => list_to_binary(AMQPPass)};
        _ ->
            #{}
    end,
    MergedRmqVars = maps:merge(RmqVars, EnvVars),
    maps:merge(FileVars, #{rabbitmq => MergedRmqVars}).


set_lager_log_level(LogLevel) ->
    Levels = [<<"debug">>, <<"info">>, <<"notice">>, <<"warning">>, <<"error">>,
              <<"critical">>, <<"alert">>, <<"emergency">>],
    case lists:member(LogLevel, Levels) of
        true ->
            lager:set_loglevel(lager_file_backend, "main.log",
                               erlang:binary_to_atom(LogLevel, latin1)),
            ok;
        false ->
            error
    end.


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
      rabbitmq => #{url => <<"localhost">>,
                    exchange => <<"repository_activity">>,
                    port => 5672,
                    user => not_given,
                    pass => not_given}}.
