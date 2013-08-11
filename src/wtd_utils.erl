%%% @author     Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       utils for wtd
%%%
%%% @end
%%% Created : 14 Jul 2013 by gordonguthrie@backawinner.gg

-module(wtd_utils).

-export([
         get_root_dir/0
        ]).

get_root_dir() ->
    filename:dirname(code:where_is_file("erlang_wtd.app")) ++ "/../".

