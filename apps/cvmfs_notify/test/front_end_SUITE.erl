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
%%-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([subscribe_wait_trigger/1,
         subscribe_wait_timeout/1,
         trigger_subscribe_nowait/1,
         trigger_subscribe_wait_trigger/1]).

-define(API_ROOT, "/api/v1").

-define(TEST_TIMEOUT, 5000).

%% Test description

all() ->
    [subscribe_wait_trigger,
     subscribe_wait_timeout,
     trigger_subscribe_nowait,
     trigger_subscribe_wait_trigger].


%% Set up and tear down

init_per_suite(Config) ->
    application:ensure_all_started(gun),

    TcpPort = 8081,
    RepoIdleTimeout = 1000,
    ok = application:load(cvmfs_notify),
    TestUserVars = #{fe_tcp_port => TcpPort},
    ok = application:set_env(cvmfs_notify, user_config, TestUserVars),
    {ok, _} = application:ensure_all_started(cvmfs_notify),
    [{http_timeout, RepoIdleTimeout * 2}] ++ Config.

end_per_suite(_Config) ->
    application:stop(cvmfs_notify),
    application:unload(cvmfs_notify),
    application:stop(gun),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    ok.


%% Test specifications

subscribe_wait_trigger(Config) ->
    Parent = self(),
    spawn_link(fun() ->
                       subscribe_wait(<<"test_repo">>, 1, Parent, Config)
               end),
    ct:sleep(500),
    trigger(<<"test_repo">>, 1, <<"abcdef">>, 1),
    receive
        #{<<"status">> := Status, <<"revision">> := Revision, <<"root_hash">> := RootHash} ->
            <<"ok">> = Status,
            1 = Revision,
            <<"abcdef">> = RootHash
    after ?TEST_TIMEOUT ->
            exit(no_reply_from_test_process)
    end.

subscribe_wait_timeout(Config) ->
    Parent = self(),
    trigger(<<"test_repo">>, 1, <<"abcdef">>, 1),
    spawn_link(fun() ->
                       subscribe_wait(<<"test_repo">>, 2, Parent, Config)
               end),
    receive
        #{<<"status">> := Status, <<"reason">> := Reason} ->
            <<"idle">> = Status,
            <<"no repo activity until timeout">> = Reason
    after ?TEST_TIMEOUT ->
            exit(no_reply_from_test_process)
    end.


trigger_subscribe_nowait(Config) ->
    Parent = self(),
    trigger(<<"test_repo">>, 1, <<"abcdef">>, 1),
    spawn_link(fun() ->
                       subscribe_wait(<<"test_repo">>, 1, Parent, Config)
               end),
    receive
        #{<<"status">> := Status, <<"revision">> := Revision, <<"root_hash">> := RootHash} ->
            <<"ok">> = Status,
            1 = Revision,
            <<"abcdef">> = RootHash
    after ?TEST_TIMEOUT ->
            exit(no_reply_from_test_process)
    end.

trigger_subscribe_wait_trigger(Config) ->
    Parent = self(),
    trigger(<<"test_repo">>, 1, <<"abcdef">>, 1),
    spawn_link(fun() ->
                       subscribe_wait(<<"test_repo">>, 2, Parent, Config)
               end),
    ct:sleep(500),
    trigger(<<"test_repo">>, 2, <<"ghijkl">>, 1),
    receive
        #{<<"status">> := Status, <<"revision">> := Revision, <<"root_hash">> := RootHash} ->
            <<"ok">> = Status,
            2 = Revision,
            <<"ghijkl">> = RootHash
    after ?TEST_TIMEOUT ->
            exit(no_reply_from_test_process)
    end.


%% Private functions

wait(ConnPid, StreamRef) ->
    case gun:await(ConnPid, StreamRef, 1000) of
        {response, nofin, _Status, _Headers} ->
            gun:await_body(ConnPid, StreamRef);
        {response, fin, _Status, _Headers} ->
            {error, reply_without_body}
    end.

subscribe_wait(Repo, MinRevision, Parent, Config) ->
    {ok, ConnPid} = gun:open("localhost", 8081),
    {ok, http} = gun:await_up(ConnPid),
    gun:ws_upgrade(ConnPid, ?API_ROOT ++ "/notify"),
    Ret = receive
              {gun_ws_upgrade, ConnPid, ok, _Headers} ->
                  Req = jsx:encode(#{<<"repo">> => Repo,
                                     <<"min_revision">> => MinRevision}),
                  gun:ws_send(ConnPid, {binary, Req}),
                  receive
                      {gun_ws, ConnPid, {binary, ReplyBin}} ->
                          jsx:decode(ReplyBin, [return_maps])
                  after ?config(http_timeout, Config) ->
                          exit(timeout)
                  end
          after ?config(http_timeout, Config) ->
                  exit(timeout)
          end,
    Parent ! Ret,
    ok = gun:shutdown(ConnPid).

trigger(Repo, Revision, RootHash, ApiVersion) ->
    Body = jsx:encode(#{<<"repo">> => Repo,
                        <<"revision">> => Revision,
                        <<"root_hash">> => RootHash,
                        <<"api_version">> => ApiVersion}),
    {ok, ConnPid} = gun:open("localhost", 8081),
    {ok, http} = gun:await_up(ConnPid),
    StreamRef = gun:post(ConnPid,
                         ?API_ROOT ++ "/trigger",
                         [{<<"content-type">>, <<"application/json">>}]),
    gun:data(ConnPid, StreamRef, fin, Body),
    wait(ConnPid, StreamRef),
    ok = gun:shutdown(ConnPid).
