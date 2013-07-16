%%% @author    A normal file with not WTD
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       Should throw compilation errors
%%%
%%% @end
%%% Created : 14 Jul 2013 by Gordon Guthrie

-module(test_compile_fail_2).

-export([
        test/0
       ]).

-wtd_export({bimbo, [
                     {"not_atom", 3},
                     {atom, "not integer"},
                     not_tuple
                    ]}).

test() ->
    {ok, test_compilation_fail_1}.
