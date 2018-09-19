%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc publish_handler - HTTP request handler for /publish
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(publish_handler).

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
init(Req0 = #{method := <<"POST">>}, State) ->
    Uid = util:unique_id(),
    {URI, T0} = util:tick(Uid, Req0, micro_seconds),

    {ok, Body, Req1} = front_end_util:read_body(Req0),

    {Status, Reply, Req2} = case jsx:decode(Body, [return_maps]) of
                                #{<<"repo">> := Repo,
                                  <<"revision">> := Revision,
                                  <<"root_hash">> := RootHash,
                                  <<"api_version">> := _} ->
                                    case event_manager:trigger(Repo, Revision, RootHash) of
                                        ok ->
                                            Rep = #{<<"status">> => <<"ok">>},
                                            {200, Rep, Req1};
                                        {error, Reason} ->
                                            lager:error("Trigger failed: ~p", [Reason]),
                                            Rep = #{<<"status">> => <<"error">>,
                                                    <<"reason">> => Reason},
                                            {400, Rep, Req1}
                                    end;
                                Other ->
                                    lager:error("Could not decode trigger request body: ~p", [Other]),
                                    {400, #{<<"status">> => <<"error">>,
                                            <<"reason">> => <<"invalid_request_body">>}, Req1}
                            end,
    ReqF = cowboy_req:reply(Status,
                            #{<<"content-type">> => <<"application/json">>},
                            jsx:encode(Reply),
                            Req2),

    util:tock(Uid, URI, T0, micro_seconds),
    {ok, ReqF, State}.
