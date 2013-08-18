%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright Goron Guthrie
%%% @doc       The Rx server
%%%            handling receiving info
%%%
%%% @end
%%% Created : 18 Aug 2013 by gordon@vixo.com
%%%-------------------------------------------------------------------
-module(rx_srv).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {proxy}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Proxy) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Proxy], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Proxy]) ->
    io:format("Initing with ~p~n", [Proxy]),
    {ok, #state{proxy = Proxy}}.

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
