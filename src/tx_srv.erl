%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright Goron Guthrie
%%% @doc       The Tx server transmitting requests
%%%
%%% @end
%%% Created : 18 Aug 2013 by gordon@vixo.com
%%%-------------------------------------------------------------------
-module(tx_srv).

-behaviour(gen_server).

%% API
-export([
         start_link/0
        ]).

-export([
         send/2
        ]).

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

-record(state, {requests = dict:new()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

send(Proxy, Request) ->
    gen_server:call(?SERVER, {send, {Proxy, Request}}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    io:format("Tx server starting...~n"),
    {ok, #state{}}.

handle_call({send, {Proxy, Request}}, _From, State) ->
    NewState = handle_send(Proxy, Request, State),
    Reply = ok,
    {reply, Reply, NewState}.

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
handle_send(Proxy, Request, State) ->
    io:format("Proxy is ~p~nRequest is ~p~n", [Proxy, Request]),
    State.
