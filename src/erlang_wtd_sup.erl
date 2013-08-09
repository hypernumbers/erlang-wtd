%%% @author     Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc
%%%
%%% @end
%%% Created : 14 Jul 2013 by gordonguthrie@backawinner.gg
-module(erlang_wtd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->

    EPMD = ?CHILD(epmd_srv, worker),

    {ok, { {one_for_one, 5, 10}, [EPMD]} }.

