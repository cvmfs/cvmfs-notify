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
%% Handles requests for the /publish end point
%%
%% Extracts the body of the messages and forwards it to the publisher
%% server
%%
%% @end
%%--------------------------------------------------------------------
init(Req0 = #{method := <<"POST">>}, State) ->
    Uid = util:unique_id(),
    {URI, T0} = util:req_tick(Uid, Req0, micro_seconds),

    {ok, Body, Req1} = read_body(Req0),

    {Status, Reply} = case message:validate(Body) of
                          {ok, Repo} ->
                              lager:debug("Publish request: repo: ~p, msg: ~p", [Repo, Body]),
                              publisher:send(Repo, Body),
                              {200, #{<<"status">> => <<"ok">>}};
                          {error, Reason} ->
                              {400, util:error_map(Reason)}
                      end,

    ReqF = cowboy_req:reply(Status,
                            #{<<"content-type">> => <<"application/json">>},
                            jsx:encode(Reply),
                            Req1),

    util:req_tock(Uid, URI, T0, micro_seconds),
    {ok, ReqF, State}.


read_body(Req0) ->
    read_body_rec(Req0, <<"">>).


read_body_rec(Req0, Acc) ->
    case cowboy_req:read_body(Req0,#{length => 1000000000, period => 36000000}) of
        {ok, Data, Req1} ->
            DataSize = size(Data),
            {ok, <<Data:DataSize/binary,Acc/binary>>, Req1};
        {more, Data, Req1} ->
            DataSize = size(Data),
            read_body_rec(Req1, <<Data:DataSize/binary,Acc/binary>>)
    end.
