%%%-------------------------------------------------------------------
%% @doc cvmfs_notify top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cvmfs_notify_sup).

-behaviour(supervisor).

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

init(_Args) ->
    SupervisorSpecs = #{strategy => one_for_all,
                        intensity => 5,
                        period => 5},
    %% WorkerSpecs = [
    %%   #{id => cvmfs_auth,
    %%     start => {cvmfs_auth, start_link, [{Repos, Keys}]},
    %%     restart => permanent,
    %%     shutdown => 2000,
    %%     type => worker,
    %%     modules => [cvmfs_auth]}
    %%  ],
    {ok, {SupervisorSpecs, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
