%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc Message-related functions
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(message).

-export([validate/1]).


-spec validate(Msg :: binary()) -> {{ok, Repo :: binary()} |
                                    {error, Reason :: binary()}}.
validate(Msg) ->
    case jsx:decode(Msg, [return_maps]) of
        #{<<"version">> := _Version,
          <<"timestamp">> := _Timestamp,
          <<"type">> := <<"activity">>,
          <<"repository">> := Repo,
          <<"manifest">> := _Manifest} ->
            {ok, Repo};
        _ ->
            {error, <<"invalid message">>}
    end.