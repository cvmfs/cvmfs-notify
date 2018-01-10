%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc
%%%
%%% @end
%%%
%%%-------------------------------------------------------------------

-module(front_end_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([check_dummy/1]).

-define(API_ROOT, "/api/v1").

%% Test description

all() ->
    [{group, dummy}].

groups() ->
    [
     {dummy, [], [check_dummy]}
    ].


%% Set up and tear down

init_per_suite(Config) ->
    application:ensure_all_started(gun),

    ok = application:load(cvmfs_notify),
    TestUserVars = #{fe_tcp_port => 8081},
    ok = application:set_env(cvmfs_notify, user_config, TestUserVars),
    {ok, _} = application:ensure_all_started(cvmfs_notify),
    Config.

end_per_suite(_Config) ->
    application:stop(cvmfs_notify),
    application:unload(cvmfs_notify),
    application:stop(gun),
    ok.

init_per_testcase(_TestCase, Config) ->
    {ok, ConnPid} = gun:open("localhost", 8081),
    {ok, http} = gun:await_up(ConnPid),
    [{gun_connection, ConnPid} | Config].

end_per_testcase(_TestCase, Config) ->
    ConnPid = ?config(gun_connection, Config),
    ok = gun:shutdown(ConnPid),
    ok.


%% Test specifications

check_dummy(Config) ->
    ConnPid = conn_pid(Config),
    gun:ws_upgrade(ConnPid, ?API_ROOT ++ "/notify"),
    receive
        {gun_ws_upgrade, ConnPid, ok, _Headers} ->
            gun:ws_send(ConnPid, {text, "Hello, server!"}),
            receive
                {gun_ws, ConnPid, Frame} ->
                    {text, <<"Hello, client!">>} = Frame
            after 1000 ->
                    exit(timeout)
            end
    after 1000 ->
            exit(timeout)
    end.


%% Private functions

conn_pid(Config) ->
    ?config(gun_connection, Config).
