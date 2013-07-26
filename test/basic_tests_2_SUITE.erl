%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Gutrie
%%% @doc
%%%
%%% @end
%%% Created : 14 Jul 2013 by gordon@vixo.com
%%%-------------------------------------------------------------------
-module(basic_tests_2_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    {ok, Dir} = file:get_cwd(),
    Dir2 = Dir ++ "/../..//test/wtd_passing",
    Ret = wtd:compile(Dir2),
    io:format("Ret is ~p~n", [Ret]),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [
     test_exporting,
     test_exporting_2,
     test_exporting_3,
     test_behaviour,
     test_no_behaviour
    ].

test_exporting() ->
    [].

test_exporting(_Config) ->
    Exp1 = wtd:is_exported(exporting, test_export, test_export, 0),
    Exp2 = wtd:is_exported(exporting, test_export, test_export, 1),
    Exp3 = wtd:is_exported(exporting, test_export, test_nonce,  0),
    io:format("Exp1 is ~p~nExp2 is ~p~nExp3 is ~p~n",
              [Exp1, Exp2, Exp3]),
    case {Exp1, Exp2, Exp3} of
        {true, true, false} -> ok;
        _                   -> exit("crunk is borked...")
    end.

test_exporting_2() ->
    [].

test_exporting_2(_Config) ->
    Exp1 = wtd:is_exported(exporting, test_export_2, test_export, 0),
    Exp2 = wtd:is_exported(exporting, test_export_2, test_export, 1),
    Exp3 = wtd:is_exported(exporting, test_export_2, test_nonce,  0),
    io:format("Exp1 is ~p~nExp2 is ~p~nExp3 is ~p~n",
              [Exp1, Exp2, Exp3]),
    case {Exp1, Exp2, Exp3} of
        {true, true, false} -> ok;
        _                   -> exit("crunk is borked...")
    end.

test_exporting_3() ->
    [].

test_exporting_3(_Config) ->
    Exp1 = wtd:is_exported(exporting,       test_export_3, test_export, 0),
    Exp2 = wtd:is_exported(exporting_again, test_export_3, test_export, 1),
    Exp3 = wtd:is_exported(exporting,       test_export_3, test_nonce,  0),
    io:format("Exp1 is ~p~nExp2 is ~p~nExp3 is ~p~n",
              [Exp1, Exp2, Exp3]),
    case {Exp1, Exp2, Exp3} of
        {true, true, false} -> ok;
        _                   -> exit("crunk is borked...")
    end.

test_behaviour() ->
    [].

test_behaviour(_Config) ->
    Exp1 = wtd:has_behaviour(exported_server,   test_wtd_server,   gen_server),
    Exp2 = wtd:has_behaviour(exported_server,   test_wtd_server_2, gen_server),
    Exp3 = wtd:has_behaviour(exported_server_2, test_wtd_server_2, gen_server),
    case {Exp1, Exp2, Exp3} of
        {true, true, true} -> ok;
        _            -> exit("crunk is borked...")
    end.

test_no_behaviour() ->
    [].

test_no_behaviour(_Config) ->
    Exp1 = wtd:has_behaviour(exported_server, test_wtd_server_3, fishbowl),
    case Exp1 of
        false -> ok;
        _     -> exit("crunk is borked...")
    end.

