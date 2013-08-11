%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       This generates the dev wtd
%%%
%%% @end
%%% Created :  3 Aug 2013 by gordonguthrie@backawinner.gg

-module(wtd_dev_compile).

-export([
         wtd_dev_compile/2
        ]).

wtd_dev_compile(_A, _B) ->
    {ok, Dir} = file:get_cwd(),
    code:add_patha(Dir ++ "/ebin"),
    ok = wtd:compile(Dir ++ "/src"),
    ok.
