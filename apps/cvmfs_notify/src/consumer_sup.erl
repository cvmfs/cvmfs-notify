%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc AMQP consumer supervisor.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(consumer_sup).

-behaviour(supervisor).

-compile([{parse_transform, lager_transform}]).

%% API
-export([start_link/1, start_consumer/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).


start_consumer(Repo) ->
    supervisor:start_child(?SERVER, [Repo]).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

init(Args) ->
    SupervisorSpecs = #{strategy => simple_one_for_one,
                        intensity => 5,
                        period => 5},
    WorkerSpecs = [
      #{id => consumer,
        start => {consumer, start_link, [Args]},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [consumer]}
     ],

    lager:info("Consumer supervisor started"),

    {ok, {SupervisorSpecs, WorkerSpecs} }.

