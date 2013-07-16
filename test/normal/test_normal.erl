%%% @author    A normal file with not WTD
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       The basic test includes checking if a single file
%%%            without any WTD addions compiles correctly
%%%
%%% @end
%%% Created : 14 Jul 2013 by Gordon Guthrie

-module(test_normal).

-export([
        test/0
       ]).


test() ->
    {ok, test_normal}.
