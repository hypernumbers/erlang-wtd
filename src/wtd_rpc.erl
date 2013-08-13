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

%%%-----------------------------------------------------------------------------
%%%
%%% API Fns
%%%
%%%-----------------------------------------------------------------------------

call(Node, Module, Fn, Arguments) when is_atom(Module)    andalso
                                       is_atom(Fn)        andalso
                                       is_list(Arguments) ->
    ok.

%%%-----------------------------------------------------------------------------
%%%
%%% Internal Fns
%%%
%%%-----------------------------------------------------------------------------
