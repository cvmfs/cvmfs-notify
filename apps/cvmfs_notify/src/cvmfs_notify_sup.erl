%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc cvmfs_notify top level supervisor.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(cvmfs_notify_sup).

-behaviour(supervisor).

-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init({Args, AMQPModule}) ->
    Credentials = maps:get(amqp, Args),

    SupervisorSpecs = #{strategy => one_for_all,
                        intensity => 5,
                        period => 5},
    WorkerSpecs = [
      #{id => cvmfs_subscriptions,
        start => {cvmfs_subscriptions, start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [cvmfs_subscriptions]},
      #{id => cvmfs_publisher,
        start => {cvmfs_publisher, start_link, [{Credentials, AMQPModule}]},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [cvmfs_publisher]},
      #{id => cvmfs_consumer_mgr,
        start => {cvmfs_consumer_mgr, start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [cvmfs_consumer_mgr]},
      #{id => cvmfs_consumer_sup,
        start => {cvmfs_consumer_sup, start_link, [{Credentials, AMQPModule}]},
        restart => permanent,
        shutdown => 2000,
        type => supervisor,
        modules => [cvmfs_consumer_sup]}
     ],

    lager:info("Main supervisor started"),

    {ok, {SupervisorSpecs, WorkerSpecs} }.

%%====================================================================
%% Internal functions
%%====================================================================
