%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc utility functions
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(util).

-compile([{parse_transform, lager_transform}]).

-export([req_tick/3
        ,req_tock/4
        ,error_map/1
        ,unique_id/0
        ,read_vars/0
        ,set_lager_log_level/1]).



-spec error_map(Reason :: atom()) -> #{ binary() => binary() }.
error_map(Reason) ->
    #{<<"status">> => <<"error">>,
      <<"reason">> => atom_to_binary(Reason, latin1)}.


unique_id() ->
    base64:encode(uuid:get_v4_urandom()).


req_tick(Uid, Req, Unit) ->
    T = erlang:monotonic_time(Unit),
    URI = cowboy_req:uri(Req),
    lager:debug("HTTP req received - Uid: ~p, URI: ~p", [Uid, URI]),
    {URI, T}.


req_tock(Uid, URI, T0, Unit) ->
    T1 = erlang:monotonic_time(Unit),
    lager:debug("HTTP req handled - Uid: ~p, URI: ~p, time to process = ~p usec",
               [Uid, URI, T1 - T0]).


read_vars() ->
    DefaultVars = default_vars(),
    FileVars = read_vars(user_config),
    EnvVars = read_env_vars(),
    maps:merge(maps:merge(DefaultVars, FileVars), EnvVars).


set_lager_log_level(LogLevel) ->
    Levels = [debug, info, notice, warning, error, critical, alert, emergency],
    case lists:member(LogLevel, Levels) of
        true ->
            lager:set_loglevel(lager_console_backend, LogLevel),
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
            V = jsx:decode(Data, [{labels, atom}, return_maps]),
            case maps:is_key(log_level, V) of
                true ->
                    LogLevel = maps:get(log_level, V),
                    maps:update(log_level, binary_to_atom(LogLevel, latin1), V);
                false ->
                    V
                end;
        {error, Reason} ->
            {error, Reason}
    end.

read_env_vars() ->
    F = fun({Key, Type}, EnvVarName, Acc) ->
        case os:getenv(EnvVarName) of
            Val when is_list(Val), length(Val) > 0 ->
                case Type of
                    int ->
                        maps:put(Key, list_to_integer(Val), Acc);
                    string ->
                        maps:put(Key, list_to_binary(Val), Acc);
                    atom ->
                        maps:put(Key, list_to_atom(Val), Acc)
                    end;
            _ ->
                Acc
            end
    end,

    RmqVars = maps:fold(F, #{}, #{{user, string} => "CVMFS_NOTIFY_AMQP_USERNAME",
                                  {pass, string} => "CVMFS_NOTIFY_AMQP_PASSWORD",
                                  {url, string} => "CVMFS_NOTIFY_AMQP_URL",
                                  {port, int} => "CVMFS_NOTIFY_AMQP_PORT",
                                  {exchange, string} => "CVMFS_NOTIFY_AMQP_EXCHANGE"}),

    MainVars = maps:fold(F, #{}, #{{port, int} => "CVMFS_NOTIFY_PORT",
                                   {testing_mode, atom} => "CVMFS_NOTIFY_TESTING_MODE",
                                   {log_level, atom} => "CVMFS_NOTIFY_LOG_LEVEL"}),

    case RmqVars of
        #{} ->
            MainVars;
        _ ->
            maps:put(amqp, RmqVars, MainVars)
        end.

default_vars() ->
    #{port => 4930,
      testing_mode => false,
      log_level => info,
      amqp => #{user => <<"">>,
                pass => <<"">>,
                url => <<"localhost">>,
                exchange => <<"repository_activity">>,
                port => 5672
      }}.