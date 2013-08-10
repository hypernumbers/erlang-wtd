-module(start).

-export([
         start/0
        ]).

start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(inets),
    ok = application:start(erlang_wtd).
