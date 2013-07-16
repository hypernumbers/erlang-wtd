%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2013, Gordon Gutrie
%%% @doc       The most primitive tests
%%%
%%% @end
%%% Created : 14 Jul 2013 by gordon@vixo.com
%%%-------------------------------------------------------------------
-module(basic_tests_1_SUITE).

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
     test_normal,
     failed_compile_0,
     failed_compile_1,
     failed_compile_2,
     failed_compile_3
    ].

test_normal() ->
    [].

test_normal(_Config) ->
    Dir = code:lib_dir(wtd),
    Dir2 = Dir ++ "/test/normal",
    ok = wtd:compile(Dir2),
    Default  = wtd:default_clef_TEST(),
    Inb  = Dir ++ "/cbin/inbound.clef",
    OutB = Dir ++ "/cbin/outbound.clef",
    {ok, Inbound}  = file:read_file(Inb),
    {ok, Outbound} = file:read_file(OutB),
    io:format("Inbound is ~p~nOutbound is ~p~nDefault is ~p~n",
                  [binary_to_list(Inbound), binary_to_list(Outbound), Default]),
    case {binary_to_list(Inbound), binary_to_list(Outbound)} of
        {Default, Default} -> ok;
        _                  -> exit("Failed to build clef files")
    end,
    file:delete(Inb),
    file:delete(OutB),
    ok.

failed_compile_0() ->
    [].

failed_compile_0(_Config) ->
    Dir = code:lib_dir(wtd),
    Dir2 = Dir ++ "/test/compile_fail_0",
    [{file_wont_compile, _}] = wtd:compile(Dir2).

failed_compile_1() ->
    [].

failed_compile_1(_Config) ->
    Dir = code:lib_dir(wtd),
    Dir2 = Dir ++ "/test/compile_fail_1",
    [{invalid_wtd_export, {bimbo, limbo, himbo}, _, _}] = wtd:compile(Dir2).

failed_compile_2() ->
    [].

failed_compile_2(_Config) ->
    Dir = code:lib_dir(wtd),
    Dir2 = Dir ++ "/test/compile_fail_2",
    [
     {not_function_and_arity, _, _, _},
     {not_function_and_arity, _, _, _},
     {not_function_and_arity, _, _, _}
    ] = wtd:compile(Dir2).

failed_compile_3() ->
    [].

failed_compile_3(_Config) ->
    Dir = code:lib_dir(wtd),
    Dir2 = Dir ++ "/test/compile_fail_3",
   [{function_not_exported, {jinko, 0}, none}] = wtd:compile(Dir2).
