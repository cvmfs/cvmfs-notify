%%%-------------------------------------------------------------------
%%% This file is part of the CernVM File System.
%%%
%%% @doc Message-related functions
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(cvmfs_message).

-export([validate/1]).


-spec validate(Msg :: binary()) -> {ok, Repo :: binary()} |
                                   {error, invalid_message}.
validate(Msg) when is_binary(Msg) ->
    case jsx:decode(Msg, [return_maps]) of
        #{<<"version">> := _Version,
          <<"timestamp">> := _Timestamp,
          <<"type">> := <<"activity">>,
          <<"repository">> := Repo,
          <<"manifest">> := _Manifest} when is_binary(Repo) ->
            {ok, Repo};
        _ ->
            {error, invalid_message}
    end.

