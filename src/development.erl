%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       A Skeleton wtd_server for testing
%%%
%%% @end
%%% Created : 14 Jul 2013 by gordon@vixo.com
%%%-------------------------------------------------------------------
-module(development).

-behaviour(gen_server).
-wtd_behaviour(development).

%% API
-export([
         start_link/0
        ]).

-export([
         test/0
         ]).

-wtd_export({development, [
                           test/0
                          ]
            }).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

test() ->
    Str = io_lib:format("development:test called on ~p at ~p~n",
                        [node(), now()]),
    io:format(Str),
    "Replying with " ++ lists:flatten(Str).

%%%===================================================================
%%% development_srv callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
