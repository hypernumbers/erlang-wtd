%%% @author     Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       The main module of Erlang WTD
%%%
%%% @end
%%% Created : 14 Jul 2013 by gordonguthrie@backawinner.gg

-module(wtd_rpc).

-export([
         call/4
        ]).

-include("wtd.hrl").

%%%-----------------------------------------------------------------------------
%%%
%%% API Fns
%%%
%%%-----------------------------------------------------------------------------

call(Node, Mod, Fn, Args) when is_atom(Mod)  andalso
                               is_atom(Fn)   andalso
                               is_list(Args) ->
    Arity = length(Args),
    case epmd_srv:get_proxy(Node, Mod, Fn, Arity) of
        {ok, {Proxy, PubK}} ->
            PrivK = epmd_srv:get_outbound_private_key(PubK),
            Request = #request{node      = Node,
                               module    = Mod,
                               function  = Fn,
                               arguments = Args},
            SignedRequest = make_signed_req(Request, PubK, PrivK),
            send_rpc(Proxy, SignedRequest);
        {error, Error} ->
            io:format("Error ~p~n", [Error]),
            {error, Error}
end.

%%%-----------------------------------------------------------------------------
%%%
%%% Internal Fns
%%%
%%%-----------------------------------------------------------------------------
make_signed_req(Request, PubK, PrivK) ->
    io:format("Request is ~p~n", [Request]),
    Req = base64:encode(bert:encode(Request)),
    Sig = wtd_utils:sign_string(Req, PrivK),
    #signed_request{public_key = PubK,
                    signature  = Sig,
                    request    = Request}.

send_rpc(Proxy, Request) when is_record(Proxy, proxy)            andalso
                              is_record(Request, signed_request) ->
    Path = "/tx/",
    BRequest = base64:encode(bert:encode(Request)),
    case wtd_utils:make_http_req(Proxy, Path, BRequest) of
        {ok, {{_, 200, _}, _, X}} -> X1 = bert:decode(base64:decode(X)),
                                     io:format("X1 is ~p~n", [X1]);
        bingo -> ok
    end.

