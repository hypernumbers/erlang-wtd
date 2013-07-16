%%% @author    A normal file with not WTD
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       Should throw compilation errors
%%%
%%% @end
%%% Created : 14 Jul 2013 by Gordon Guthrie

-module(test_compile_fail_1).

-export([
        test/0
       ]).

-wtd_export({bimbo, limbo, himbo}).

test() ->
    {ok, test_compilation_fail_1}.
