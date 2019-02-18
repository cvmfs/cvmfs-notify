%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc cvmfs_notify application
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(cvmfs_notify_app).

-compile([{parse_transform, lager_transform}]).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    UserVars = cvmfs_util:read_vars(),

    LogLevel = maps:get(log_level, UserVars),
    ok = cvmfs_util:set_lager_log_level(LogLevel),

    TcpPort = maps:get(port, UserVars),
    {ok, _} = cvmfs_front_end:start_link([TcpPort]),

    % Use the mocked AMQP interface for testing?
    AMQPModule = case maps:get(testing_mode, UserVars, false) of
      false ->
        cvmfs_amqp_interface;
      true ->
        cvmfs_mock_amqp_interface
    end,

    cvmfs_notify_sup:start_link({UserVars, AMQPModule}).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

