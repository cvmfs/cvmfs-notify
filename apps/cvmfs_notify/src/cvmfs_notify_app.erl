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
    UserVars = util:read_vars(user_config, util:default_config()),

    AMQPUser = os:getenv("CVMFS_NOTIFY_AMQP_USERNAME"),
    AMQPPass = os:getenv("CVMFS_NOTIFY_AMQP_PASSWORD"),

    case lists:any(fun(V) -> (V =:= false) or (V =:= []) end, [AMQPUser, AMQPPass]) of
        true ->
            lager:error("AMQP credentials not found. " ++
                        "Please set the CVMFS_NOTIFY_AMQP_USERNAME and " ++
                        "CVMFS_NOTIFY_AMQP_PASSWORD environment variables"),
            exit(missing_credentials);
        _ ->
            ok
    end,

    FinalUserVars = maps:merge(UserVars, #{amqp_user => AMQPUser, amqp_pass => AMQPPass}),

    TcpPort = maps:get(port, FinalUserVars),
    {ok, _} = front_end:start_link([TcpPort]),

    lager:info("User vars: ~p", [UserVars]),

    cvmfs_notify_sup:start_link(FinalUserVars).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

