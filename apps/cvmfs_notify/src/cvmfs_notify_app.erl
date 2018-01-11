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
    UserVars = read_vars(user_config, #{fe_tcp_port => 8081,
                                        repo_idle_timeout => 30000}),

    TcpPort = maps:get(fe_tcp_port, UserVars),
    RepoIdleTimeout = maps:get(repo_idle_timeout, UserVars),
    application:set_env(cvmfs_notify,
                        repo_idle_timeout,
                        RepoIdleTimeout),
    {ok, _} = front_end:start_link([TcpPort]),

    lager:info("User vars: ~p", [UserVars]),

    cvmfs_notify_sup:start_link([]).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
read_vars(VarName, Defaults) ->
    case application:get_env(VarName) of
        {ok, {file, ConfigFile}} ->
            {ok, VarList} = file:consult(ConfigFile),
            maps:from_list(VarList);
        {ok, ConfigMap} ->
            ConfigMap;
        undefined ->
            Defaults
    end.
