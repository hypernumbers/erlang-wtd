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
         tick/0,
         get_connections/0
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
          wtd_node,
          status
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

get_connections() ->
    gen_server:call(?SERVER, get_connections).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, Config} = application:get_env(erlang_wtd, epmds),
    {ok, {Public, WTDNode}} = application:get_env(erlang_wtd, wtdnodename),
    EPMDs = [make_epmd(Public, WTDNode, X) || X <- Config],
    {ok, _} = timer:apply_after(?TICK, epmd_srv, tick, []),
    {ok, #state{epmds = EPMDs}}.

handle_call(tick, _From, State) ->
    #state{epmds = EPMDs} = State,
    Reply = ok,
    {ok, _} = timer:apply_after(?TICK, epmd_srv, tick, []),
    NewEPMDS = [ping(X) || X <- EPMDs],
    {reply, Reply, State#state{epmds = NewEPMDS}};
handle_call(get_connections, _From, State) ->
    #state{epmds = EPMDs} = State,
    Reply = [{N, St} || #epmd{name = N, status = St} <- EPMDs],
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
    Body = jiffy:encode({[
                          {name, "myname"},
                          {vals, "somevals"}
                         ]}),
    Path = "/",
    HTTPAuthHeader = hmac_api_lib:sign(PrivateKey, PublicKey, Method, Path,
                                       Headers, ContentType),

    Request = {URL ++ Path, [HTTPAuthHeader | Headers], ContentType, Body},
    {Code, R2} = case httpc:request(Method, Request, [], []) of
                     {ok, {{_, Cd, _}, _, R}} -> {Cd, jiffy:decode(R)};
                     _                        -> {500, remote_error}
                 end,
    Status = case {Code, R2} of
                 {200, {[{<<"ok">>, <<"authenticated">>}]}} ->
                     authenticated;
                 {403, {[{<<"error">>, Error}]}} ->
                     list_to_atom(binary_to_list(Error));
                 {500, remote_error} ->
                     remote_error;
                 O ->
                     io:format("Connection error ~p~n", [O]),
                     other_error
             end,
    EPMD#epmd{status = Status}.

make_epmd(PublicKey, WTDNode, {Name, List}) ->
    {domain,      DM}         = lists:keyfind(domain,      1, List),
    {epmd_port,   EPMDPort}   = lists:keyfind(epmd_port,   1, List),
    {proxy_port,  ProxyPort}  = lists:keyfind(proxy_port,  1, List),
    {private_key, PrivateKey} = lists:keyfind(private_key, 1, List),
    #epmd{name        = Name,
          domain      = DM,
          epmd_port   = EPMDPort,
          proxy_port  = ProxyPort,
          public_key  = PublicKey,
          private_key = PrivateKey,
          wtd_node    = WTDNode}.
