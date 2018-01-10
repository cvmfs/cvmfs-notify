%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc trigger_handler - trigger request handler
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(trigger_handler).

-compile([{parse_transform, lager_transform}]).

-export([init/2]).


%%--------------------------------------------------------------------
%% @doc
%% Handles requests for the /trigger end point
%%
%% Properly authorized requests will trigger a notification to any
%% interested clients connected to the /notify end point
%%
%% @end
%%--------------------------------------------------------------------
init(Req0, State) ->
    {ok, cowboy_req:reply(400, Req0), State}.
