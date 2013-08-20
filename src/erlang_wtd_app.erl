%%% @author     Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc
%%%
%%% @end
%%% Created : 14 Jul 2013 by gordonguthrie@backawinner.gg
-module(erlang_wtd_app).

-behaviour(application).

%% Application callbacks
-export([
         start/0, start/2,
         stop/1
        ]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
    start([], []).

start(_StartType, _StartArgs) ->
    {ok, _PID} = erlang_wtd_sup:start_link().

stop(_State) ->
    ok.
