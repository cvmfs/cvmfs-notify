%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc Mock interface to AMQP client library
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(mock_amqp_interface).

-compile([{parse_transform, lager_transform}]).

-export([connect/1, publish/3, consume/2, handle/3]).


connect(_) ->
    #{}.

publish(Repo, Msg, _) ->
    subscriptions:notify(Repo, Msg),
    ok.

consume(_, _) ->
    ok.

handle(_, _, _) ->
    ok.