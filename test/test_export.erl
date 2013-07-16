%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Guthrie
%%% @doc
%%%
%%% @end
%%% Created : 14 Jul 2013 by gordon@vixo.com

-module(test_export).

-export([
         test_export/0,
         test_export/1,
         test_nonce/0
        ]).

-wtd_export({exporting, [
                      test_export/0
                     ]}).

-wtd_export({exporting, [
                      test_export/1
                     ]}).

test_nonce() -> {error, test_nonce}.

test_export() -> {ok, 'test_export/0'}.

test_export(_) -> {ok, 'test_export/1'}.
