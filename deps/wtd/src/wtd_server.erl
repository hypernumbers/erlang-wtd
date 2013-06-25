-module(wtd_server).

-export([behaviour_info/1]).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].

behaviour_info(callbacks) ->
    [{init,1},{handle_call,3},{handle_cast,2},{handle_info,2},
     {terminate,2},{code_change,3}];
behaviour_info(_Other) ->
    undefined.
