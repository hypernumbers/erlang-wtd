%%% @author    A normal file with not WTD
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       Should throw compilation errors
%%%
%%% @end
%%% Created : 14 Jul 2013 by Gordon Guthrie

-module(test_compile_fail_0).

-export([
        test/0
       ]).

%% can't compile
test() ->
    {ok, test_compilation_
