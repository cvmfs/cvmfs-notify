%%%-------------------------------------------------------------------
%% @doc front_end public API
%% @end
%%%-------------------------------------------------------------------

-module(front_end).

-compile([{parse_transform, lager_transform}]).

-export([start_link/1, api_version/0]).

-define(API_VERSION, 1).
-define(API_ROOT, "/api/v" ++ integer_to_list(?API_VERSION)).
-define(CONNECTION_TIMEOUT, 7200).

%%--------------------------------------------------------------------
%% @doc
%% Starts the front-end HTTP listener process.
%%
%% @end
%%--------------------------------------------------------------------
start_link([TcpPort]) ->
    Dispatch = cowboy_router:compile([{'_', [
                                             {?API_ROOT, base_handler, []}
                                            ]}]),

    %% Start the HTTP listener process configured with the routing table
    lager:info("Starting HTTP front-end"),
    cowboy:start_clear(front_end,
                       [{port, TcpPort}],
                       #{env => #{dispatch => Dispatch},
                         idle_timeout => ?CONNECTION_TIMEOUT,
                         inactivity_timeout => ?CONNECTION_TIMEOUT}).


-spec api_version() -> integer().
api_version() ->
    ?API_VERSION.

