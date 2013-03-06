-module(robotarmy_app).

-behaviour(application).

-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

start() -> start(minions, myrmidons).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    robotarmy_sup:start_link().

stop(_State) ->
    ok.
