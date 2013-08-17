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
-wtd_behaviour(dev_mission).

%% API
-export([
         start_link/0
        ]).

-export([
         test/0
         ]).

%% exports for testing
-export([
         rpc1/0,
         rpc2/0,
         failing_rpc1/0,
         failing_rpc2/0,
         failing_rpc3/0,
         failing_rpc4/0
        ]).

-wtd_export({dev_mission,
             [
              test/0,
              test/8
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

rpc1() ->
    Node = {"gordonspi@vixo.com", "dev lappie"},
    Mod  = development,
    Fn   = test,
    Args = [],
    wtd_rpc:call(Node, Mod, Fn, Args).

rpc2() ->
    Node = {"gordonspi@vixo.com", "dev lappie"},
    Mod  = development,
    Fn   = test,
    Args = [atom, true, "string", 1, 2.3, [a, b, 3], {i, am, a, tuple}],
    wtd_rpc:call(Node, Mod, Fn, Args).

failing_rpc1() ->
    Node = {"fail@vixo.com", "dev lappie"},
    Mod  = development,
    Fn   = test,
    Args = [],
    wtd_rpc:call(Node, Mod, Fn, Args).

failing_rpc2() ->
    Node = {"gordonspi@vixo.com", "dev lappie"},
    Mod  = fail,
    Fn   = test,
    Args = [],
    wtd_rpc:call(Node, Mod, Fn, Args).

failing_rpc3() ->
    Node = {"gordonspi@vixo.com", "dev lappie"},
    Mod  = development,
    Fn   = test,
    Args = [fail],
    wtd_rpc:call(Node, Mod, Fn, Args).

failing_rpc4() ->
    Node = {"gordonspi@vixo.com", "dev lappie"},
    Mod  = development,
    Fn   = test,
    Args = [],
    wtd_rpc:call(Node, Mod, Fn, Args).


test() ->
    Str = io_lib:format("development:test called on ~p at ~p~n",
                        [node(), now()]),
    io:format(Str),
    "Replying with " ++ lists:flatten(Str).

test(Atom, Boolean, String, Integer, Float, List, Tuple, Pid)
when is_atom(Atom)       andalso
     is_boolean(Boolean) andalso
     is_list(String)     andalso
     is_integer(Integer) andalso
     is_float(Float)     andalso
     is_list(List)       andalso
     is_tuple(Tuple)     andalso
     is_pid(Pid) ->
    Str = io_lib:format("development:test called on ~p at ~p~n" ++
                            "Atom    : ~p~n" ++
                            "Boolean : ~p~n" ++
                            "String  : ~p~n" ++
                            "Integer : ~p~n" ++
                            "Float   : ~p~n" ++
                            "List    : ~p~n" ++
                            "Tuple   : ~p~n" ++
                            "Pid     : ~p~n",
                        [node(), now(), Atom, Boolean, String,
                        Integer, Float, List, Tuple, Pid]),
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
