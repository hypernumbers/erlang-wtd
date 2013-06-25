-module(wtd_fsm).
-export([behaviour_info/1]).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].

behaviour_info(callbacks) ->
    [{init,1},{handle_event,3},{handle_sync_event,4},{handle_info,3},
     {terminate,3},{code_change,4}];
behaviour_info(_Other) ->
    undefined.
