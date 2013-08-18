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
         get_nodes/0,
         get_connections/0,
         get_exported_missions/0,
         get_available_missions/0,
         get_proxy/4,
         get_proxy/2,
         get_proxies/0,
         get_outbound_private_key/1,
         get_inbound_private_key/1
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

-include("wtd.hrl").

-record(epmd, {proxy,
               available_missions,
               status}).

-record(mission, {name,
                  public_key,
                  exports    = [],
                  behaviours = []}).

-record(state, {name,
                epmds,
                exported_missions,
                outbound_clefs,
                inbound_clefs}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

tick() ->
    gen_server:cast(?SERVER, tick).

get_nodes() ->
    gen_server:call(?SERVER, get_nodes).

get_connections() ->
    gen_server:call(?SERVER, get_connections).

get_exported_missions() ->
    gen_server:call(?SERVER, get_exported_missions).

get_available_missions() ->
    gen_server:call(?SERVER, get_available_missions).

get_proxy(Node, Module, Fn, Arity) ->
    gen_server:call(?SERVER, {get_proxy, {Node, atom_to_list(Module),
                                          atom_to_list(Fn), Arity}}).

get_proxy(Node, Behaviour) ->
    gen_server:call(?SERVER, {get_proxy, {Node, Behaviour}}).

get_proxies() ->
    gen_server:call(?SERVER, get_proxies).

get_outbound_private_key(PublicKey) ->
    gen_server:call(?SERVER, {get_private_key, {outbound, PublicKey}}).

get_inbound_private_key(PublicKey) ->
    gen_server:call(?SERVER, {get_private_key, {inbound, PublicKey}}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Name      = get_name(),
    EPMDs     = load_epmds(),
    Missions  = load_exported_missions(),
    {In, Out} = load_clefs(),
    {ok, _} = timer:apply_after(?TICK, epmd_srv, tick, []),
    {ok, #state{name              = Name,
                epmds             = EPMDs,
                exported_missions = Missions,
                inbound_clefs     = In,
                outbound_clefs    = Out}}.

handle_call(get_nodes, _From, State) ->
    #state{epmds = EPMDs} = State,
    Reply = [dict:fetch_keys(AM) ||
                #epmd{available_missions = AM,
                      status             = St} <- EPMDs,
                                                  St =:= authenticated],
    {reply, lists:usort(lists:flatten(Reply)), State};
handle_call(get_connections, _From, State) ->
    #state{epmds = EPMDs} = State,
    Reply = [{P#proxy.name, St} || #epmd{proxy = P,
                                         status = St} <- EPMDs],
    {reply, Reply, State};
handle_call(get_exported_missions, _From, State) ->
    #state{exported_missions = Missions} = State,
    Reply = [{N, Pk} || #mission{name = N,
                                 public_key = Pk} <- Missions],
    {reply, Reply, State};
handle_call(get_available_missions, _From, State) ->
    #state{epmds = EPMDs} = State,
    Reply = EPMDs,
    {reply, Reply, State};
handle_call({get_proxy, Resource}, _From, State) ->
    #state{epmds          = EPMDs,
           outbound_clefs = OClefs} = State,
    Reply = extract_proxy(EPMDs, OClefs, Resource),
    {reply, Reply, State};
handle_call(get_proxies, _From, State) ->
    #state{epmds = EPMDs} = State,
    Reply = [E#epmd.proxy || E <- EPMDs],
    {reply, Reply, State};
handle_call({get_private_key, {Type, PublicKey}}, _From, State) ->
    #state{inbound_clefs  = IClefs,
           outbound_clefs = OClefs} = State,
    Clefs = case Type of
                inbound  -> IClefs;
                outbound -> OClefs
            end,
    {PublicKey, Reply} = lists:keyfind(PublicKey, 1, Clefs),
    {reply, Reply, State}.

handle_cast(tick, State) ->
    #state{name              = Name,
           exported_missions = Missions,
           epmds             = EPMDs} = State,
    {ok, _} = timer:apply_after(?TICK, epmd_srv, tick, []),
    NewEPMDs = [ping(Name, Missions, X) || X <- EPMDs],
    ok = sync_rx(NewEPMDs),
    {noreply, State#state{epmds = NewEPMDs}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

ping({Email, Name}, Missions, #epmd{proxy = P} = EPMD) ->
    Path = "/",
    Body = base64:encode(bert:encode({{Email, Name}, Missions})),
    {Code, R2} = case  wtd_utils:make_http_req(P, Path, Body) of
                     {ok, {{_, Cd, _}, _, R}} ->
                         {Cd, bert:decode(base64:decode(R))};
                     RemErr ->
                         io:format("Remote Error is ~p~n", [RemErr]),
                         {500, remote_error}
                 end,
    {Status, Ms} = case {Code, R2} of
                       {200, {ok, AvailableMissions}} ->
                           {authenticated, AvailableMissions};
                       {403, {error, Error}} ->
                           io:format("Error is ~p~n", [Error]),
                           {list_to_atom(Error), dict:new()};
                       {500, remote_error} ->
                           {remote_error, dict:new()};
                       O ->
                           io:format("Connection error ~p~n", [O]),
                           {other_error, dict:new()}
                   end,
    EPMD#epmd{status = Status, available_missions = Ms}.

get_name() ->
    {ok, Wtdname} = application:get_env(erlang_wtd, wtdname),
    Wtdname.

load_epmds() ->
    {ok, Config} = application:get_env(erlang_wtd, epmds),
    {ok, {PublicKey, WTDNode}} = application:get_env(erlang_wtd, wtdname),
    _EPMDs = [make_epmd({PublicKey, WTDNode}, X) || X <- Config].

load_exported_missions() ->
    Dir = wtd_utils:get_root_dir(),
    {ok, Missions} = file:consult(Dir ++ "/cbin/missions.wtd"),
    {ok, Crunk}    = file:consult(Dir ++ "/cbin/wtd.crunk"),
    Ms = [#mission{name = N, public_key = P} || {N, P} <- Missions],
    [merge(X, Crunk) || X <- Ms].

load_clefs() ->
    Dir = wtd_utils:get_root_dir(),
    {ok, In}  = file:consult(Dir ++ "/cbin/inbound.clef"),
    {ok, Out} = file:consult(Dir ++ "/cbin/outbound.clef"),
    {In, Out}.

make_epmd({PublicKey, WTDName}, {Name, List}) ->
    {domain,      DM}         = lists:keyfind(domain,      1, List),
    {epmd_port,   EPMDPort}   = lists:keyfind(epmd_port,   1, List),
    {private_key, PrivateKey} = lists:keyfind(private_key, 1, List),
    Proxy = #proxy{name        = Name,
                   domain      = DM,
                   epmd_port   = EPMDPort,
                   private_key = PrivateKey,
                   wtd_node    = {PublicKey, WTDName}},
    #epmd{proxy = Proxy}.

merge(#mission{name = N} = M, List) ->
    case lists:keyfind(N, 1, List) of
        false  -> M;
        {N, V} -> merge2(V, M)
    end.

merge2([], Mission) -> Mission;
merge2([{behaviour, Module, Behaviour} | T], M) ->
    #mission{behaviours = Bs} = M,
    merge2(T, M#mission{behaviours = [{Module, Behaviour} | Bs]});
merge2([{export, Module, Fns} | T], M) ->
    #mission{exports = Es} = M,
    merge2(T, M#mission{exports = [{Module, Fns} | Es]}).

extract_proxy([], _OutboundClefs, _Resource) ->
    {error, none};
extract_proxy([H  | T], OutboundClefs, Resource) ->
    #epmd{available_missions = AM,
          status             = Status} = H,
    case Status of
        authenticated ->
            case in_missions(AM, OutboundClefs, Resource) of
                {true, M}  -> {ok, {H#epmd.proxy, M}};
                false      -> extract_proxy(T, OutboundClefs, Resource)
            end;
        Other -> {error, Other}
    end.

in_missions([], _, _) ->
    false;
in_missions([{Node, {AllMs, _}} | T], OutboundClefs, {Node, Mod, Fn, Arity}) ->
    Missions = matching(AllMs, OutboundClefs),
    case exports(Missions, Mod, Fn, Arity) of
        false -> in_missions(T, OutboundClefs, {Node, Mod, Fn, Arity});
        M     -> {true, M}
    end;
in_missions([{Node, {AllMissions, _}} | T], OutboundClefs, {Node, Behaviour}) ->
    %% Missions = matching(AllMissions, OutboundClefs),
    %% io:format("AllMissions is ~p~nMissions is ~p~nOutboundClefs is ~p~n",
    %%           [AllMissions, Missions, OutboundClefs]),
    in_missions(T, OutboundClefs, {Node, Behaviour});
in_missions([_H | T], OutboundClefs, Resource) ->
    in_missions(T, OutboundClefs, Resource).

matching(Missions, OutboundClefs) ->
    matching2(Missions, OutboundClefs, []).

matching2([], _OutboundClefs, Acc) ->
    lists:reverse(Acc);
matching2([#mission{public_key = PubK} = M | T], OutboundClefs, Acc) ->
    NewAcc  = case lists:keyfind(PubK, 1, OutboundClefs) of
                  {PubK, _PrivK} -> [M | Acc];
                  false          -> Acc
              end,
    matching2(T, OutboundClefs, NewAcc).

exports([], _Mod, _Fn, _Arity) ->
    false;
exports([#mission{public_key = PubK,
                  exports    = Ex} | T], Mod, Fn, Arity) ->
    case lists:keyfind(Mod, 1, Ex) of
        {Mod, Fns}  -> case lists:member({Fn, Arity}, Fns) of
                           true  -> PubK;
                           false -> exports(T,Mod, Fn, Arity)
                       end;
        false       -> exports(T, Mod, Fn, Arity)
    end.

sync_rx(EPMDs) ->
    Proxies = [E#epmd.proxy || E <- EPMDs,
                               E#epmd.status =:= authenticated],
    RxSrvs = [Name || {Name, _ , _ , _} <- supervisor:which_children(rx_sup)],
    Start = not_started(Proxies, RxSrvs),
    Stop  = not_connected(Proxies, RxSrvs),
    ok = rx_start(Start),
    ok = rx_stop(Stop).

not_started(Proxies, RxSrvs) ->
    not_st2(Proxies, RxSrvs, []).

not_st2([], _RxSrvs, Acc) ->
    lists:reverse(Acc);
not_st2([#proxy{name = N} = H | T], RxSrvs, Acc) ->
    case lists:member(N, RxSrvs) of
        true  -> not_st2(T, RxSrvs, Acc);
        false -> not_st2(T, RxSrvs, [H | Acc])
    end.

not_connected(Proxies, RxSrvs) ->
    not_cn2(RxSrvs, Proxies, []).

not_cn2([], _Proxies, Acc) ->
    lists:reverse(Acc);
not_cn2([H | T], Proxies, Acc) ->
    case lists:keymember(H, #proxy.name, Proxies) of
        true  -> not_cn2(T, Proxies, Acc);
        false -> not_cn2(T, Proxies, [H | Acc])
    end.

rx_start([]) ->
    ok;
rx_start([#proxy{name = N} = H | T]) ->
    Spec = {N, {rx_srv, start_link, [H]},
            permanent,
            5000,
            worker,
            [rx_srv]},
    {ok, _Pid} = supervisor:start_child(rx_sup, Spec),
    rx_start(T).

rx_stop([]) ->
    ok;
rx_stop([H | T]) ->
    ok = supervisor:terminate_child(rx_sup, H),
    ok = supervisor:delete_child(rx_sup, H),
    rx_stop(T).
