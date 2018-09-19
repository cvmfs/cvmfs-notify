%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc front_end public API
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(front_end).

-compile([{parse_transform, lager_transform}]).

-export([start_link/1, api_version/0]).

-define(API_VERSION, 1).
-define(API_ROOT, "/api/v" ++ integer_to_list(?API_VERSION)).

-define(TIMEOUT, 300000).

%%--------------------------------------------------------------------
%% @doc
%% Starts the front-end HTTP listener process.
%%
%% @end
%%--------------------------------------------------------------------
start_link([TcpPort]) ->
    Dispatch = cowboy_router:compile([{'_', [
                                             {?API_ROOT ++ "/subscribe",
                                              subscribe_handler,
                                              []},
                                             {?API_ROOT ++ "/publish",
                                              publish_handler,
                                              []}
                                            ]}]),

    %% Start the HTTP listener process configured with the routing table
    lager:info("Starting HTTP front-end"),
    cowboy:start_clear(front_end,
                       [{port, TcpPort}],
                       #{env => #{dispatch => Dispatch},
                         idle_timeout => ?TIMEOUT,
                         inactivity_timeout => ?TIMEOUT}).


-spec api_version() -> integer().
api_version() ->
    ?API_VERSION.

