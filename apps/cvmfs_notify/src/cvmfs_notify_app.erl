%%%-------------------------------------------------------------------
%% @doc cvmfs_notify public API
%% @end
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

    TcpPort = maps:get(port, UserVars),
    {ok, _} = front_end:start_link([TcpPort]),

    lager:info("User vars: ~p", [UserVars]),

    cvmfs_notify_sup:start_link([]).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

