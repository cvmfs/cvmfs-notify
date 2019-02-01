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
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([subscribe_wait_publish/1,
         publish_subscribe_nowait/1,
         publish_subscribe_wait_publish/1]).

-define(API_ROOT, "/api/v1").
-define(API_VER, 1).

-define(TEST_TIMEOUT, 5000).

-define(PORT, 4930).

-define(TEST_REPO, <<"test.repo.org">>).
-define(TEST_MANIFEST, <<"abcdef">>).

%% Test description

all() ->
    [subscribe_wait_publish,
     publish_subscribe_nowait,
     publish_subscribe_wait_publish].


%% Set up and tear down

init_per_suite(Config) ->
    application:ensure_all_started(gun),

    ok = application:load(cvmfs_notify),
    ok = application:set_env(cvmfs_notify, user_config, #{testing_mode => true}),
    {ok, _} = application:ensure_all_started(cvmfs_notify),
    Config.

end_per_suite(_Config) ->
    application:stop(cvmfs_notify),
    application:unload(cvmfs_notify),
    application:stop(gun),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.


%% Test specifications

subscribe_wait_publish(Config) ->
    Parent = self(),
    spawn_link(fun() ->
                       subscribe_wait(?TEST_REPO, Parent, Config)
               end),
    ct:sleep(500),
    publish(?TEST_REPO, ?TEST_MANIFEST),
    receive
        #{<<"version">> := ?API_VER,
          <<"timestamp">> := _Timestamp,
          <<"repository">> := Repo,
          <<"manifest">> := Manifest} ->
            Repo = ?TEST_REPO,
            Manifest = ?TEST_MANIFEST
    after ?TEST_TIMEOUT ->
            exit(no_reply_from_test_process)
    end.

publish_subscribe_nowait(Config) ->
    Parent = self(),
    publish(?TEST_REPO, ?TEST_MANIFEST),
    spawn_link(fun() ->
                       subscribe_wait(?TEST_REPO, Parent, Config)
               end),
    receive
        #{<<"version">> := ?API_VER,
          <<"timestamp">> := _Timestamp,
          <<"repository">> := Repo,
          <<"manifest">> := Manifest} ->
            Repo = ?TEST_REPO,
            Manifest = ?TEST_MANIFEST
    after ?TEST_TIMEOUT ->
            exit(no_reply_from_test_process)
    end.

publish_subscribe_wait_publish(Config) ->
    Parent = self(),
    publish(?TEST_REPO, ?TEST_MANIFEST),
    spawn_link(fun() ->
                       subscribe_wait(?TEST_REPO, Parent, Config)
               end),
    receive
        #{<<"manifest">> := Manifest1} ->
            Manifest1 = ?TEST_MANIFEST
    after ?TEST_TIMEOUT ->
            exit(no_reply_from_test_process)
    end,
    ct:sleep(500),
    spawn_link(fun() ->
                       subscribe_wait(?TEST_REPO, Parent, Config)
               end),
    publish(?TEST_REPO, <<"ghijkl">>),
    receive
        #{<<"manifest">> := Manifest2} ->
            Manifest2 = <<"ghijkl">>
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

subscribe_wait(Repo, Parent, _Config) ->
    {ok, ConnPid} = gun:open("localhost", ?PORT),
    {ok, http} = gun:await_up(ConnPid),
    gun:ws_upgrade(ConnPid, ?API_ROOT ++ "/subscribe"),
    Ret = receive
              {gun_upgrade, ConnPid, _Ref, [<<"websocket">>], _Headers} ->
                  Req = jsx:encode(#{<<"version">> => ?API_VER,
                                     <<"repository">> => Repo}),
                  gun:ws_send(ConnPid, {binary, Req}),
                  receive
                      {gun_ws, ConnPid, _Ref, {binary, ReplyBin}} ->
                          jsx:decode(ReplyBin, [return_maps])
                  after ?TEST_TIMEOUT ->
                      exit(timeout)
                  end
          after ?TEST_TIMEOUT ->
                  exit(timeout)
          end,
    Parent ! Ret,
    ok = gun:shutdown(ConnPid).

publish(Repo, Manifest) ->
    Body = jsx:encode(#{<<"version">> => ?API_VER,
                        <<"timestamp">> => <<"now">>,
                        <<"type">> => <<"activity">>,
                        <<"repository">> => Repo,
                        <<"manifest">> => Manifest}),
    {ok, ConnPid} = gun:open("localhost", ?PORT),
    {ok, http} = gun:await_up(ConnPid),
    StreamRef = gun:post(ConnPid,
                         ?API_ROOT ++ "/publish",
                         [{<<"content-type">>, <<"application/json">>}]),
    gun:data(ConnPid, StreamRef, fin, Body),
    wait(ConnPid, StreamRef),
    ok = gun:shutdown(ConnPid).
