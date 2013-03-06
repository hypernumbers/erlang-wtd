-module(mwaahah).

-mission({baddy, [kill/1]}).
-mission({baddy, [kill/0]}).

-export([
         kill/0,
         kill/1
        ]).

-export([
         kill/2
        ]).

-spec kill() -> ok.
kill() ->
    ok.

-spec kill(atom()) -> ok.
kill(_Arg) ->
    ok.

-spec kill(list(), integer()) -> ok.
kill(_Arg1, _Arg2) ->
    ok.
