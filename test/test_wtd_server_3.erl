%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc       A Skeleton wtd_server for testing
%%%
%%% @end
%%% Created : 14 Jul 2013 by gordon@vixo.com
%%%-------------------------------------------------------------------
-module(test_wtd_server_3).

-behaviour(fishbowl).
-wtd_behaviour(exported_server).

-export([
         bimbo/0
        ]).

bimbo() ->
    himbo.
