%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       The gen server responsible for announcing
%%%            that the server is available to the epmd service
%%%
%%% @end
%%% Created :  9 Aug 2013 by gordonguthrie@backawinner.gg
%%%-------------------------------------------------------------------
-module(epmd_srv).

-behaviour(gen_server).

%% API
-export([
         start_link/0
        ]).

%% API
-export([
         tick/0
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
-define(TICK,   5000).

-record(epmd, {
          name,
          domain,
          epmd_port,
          proxy_port,
          public_key,
          private_key,
          is_connected
         }).

-record(state, {
          epmds
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


tick() ->
    gen_server:call(?SERVER, tick).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, Config} = application:get_env(erlang_wtd, epmds),
    EPMDs = [make_epmd(X) || X <- Config],
    {ok, _} = timer:apply_after(?TICK, epmd_srv, tick, []),
    {ok, #state{epmds = EPMDs}}.

handle_call(tick, _From, State) ->
    #state{epmds = EPMDs} = State,
    Reply = ok,
    {ok, _} = timer:apply_after(?TICK, epmd_srv, tick, []),
    NewEPMDS = [ping(X) || X <- EPMDs],
    {reply, Reply, State#state{epmds = NewEPMDS}}.

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

ping(#epmd{domain      = Domain,
           epmd_port   = EPMDPort,
           public_key  = PublicKey,
           private_key = PrivateKey} = EPMD) ->
    URL = "http://" ++ Domain ++ ":" ++ integer_to_list(EPMDPort),
    Method = post,
    Date = dh_date:format("D, j M Y H:i:s"),
    Headers = [{"content-type", "application/json"},
               {"date",         Date},
               {"accept",       "application/json"}],
    ContentType = "application/json",
    Body = "ping",
    Path = "/",
    HTTPAuthHeader = hmac_api_lib:sign(PrivateKey, Method, Path,
                                       Headers, ContentType),
    IsConnected
        = case httpc:request(Method, {URL ++ Path, [HTTPAuthHeader | Headers],
                                      ContentType, Body}, [], []) of
              {ok, _} -> true;
              _       -> false
          end,
    EPMD#epmd{is_connected = IsConnected}.

make_epmd({Name, List}) ->
    {domain,      DM}         = lists:keyfind(domain,      1, List),
    {epmd_port,   EPMDPort}   = lists:keyfind(epmd_port,   1, List),
    {proxy_port,  ProxyPort}  = lists:keyfind(proxy_port,  1, List),
    {public_key,  PublicKey}  = lists:keyfind(public_key,  1, List),
    {private_key, PrivateKey} = lists:keyfind(private_key, 1, List),
    #epmd{name        = Name,
          domain      = DM,
          epmd_port   = EPMDPort,
          proxy_port  = ProxyPort,
          public_key  = PublicKey,
          private_key = PrivateKey}.
