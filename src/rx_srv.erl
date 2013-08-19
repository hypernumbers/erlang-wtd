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
-export([
         start_link/1
        ]).

-export([
         connect/0
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

-record(state, {proxy,
                wtdname}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Proxy) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Proxy], []).

connect() ->
    gen_server:cast(?SERVER, connect).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Proxy]) ->
    {ok, Wtdname} = application:get_env(erlang_wtd, wtdname),
    timer:apply_after(250, rx_srv, connect, []),
    {ok, #state{proxy   = Proxy,
                wtdname = Wtdname}}.

handle_call(_Msg, _From, State) ->

    Reply = ok,
    {reply, Reply, State}.

handle_cast(connect, #state{proxy   = P,
                            wtdname = Wtdname} = State) ->
    io:format("State is ~p~n", [State]),
    Body = base64:encode(bert:encode(Wtdname)),
    Path = "/rx/",
    {Code, R2} = case  wtd_utils:make_http_req(P, Path, Body) of
                     {ok, {{_, Cd, _}, _, R}} ->
                         {Cd, bert:decode(base64:decode(R))};
                     RemErr ->
                         io:format("Remote Error is ~p~n", [RemErr]),
                         {500, remote_error}
                 end,
    io:format("Code is ~p~nR2 is ~p~n", [Code, R2]),
    timer:apply_after(2000, rx_srv, connect, []),
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
