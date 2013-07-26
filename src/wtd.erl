%%% @author     Gordon Guthrie
%%% @copyright (C) 2013, gordon@vixo.com
%%% @doc       The main module of Erlang WTD
%%%
%%% @end
%%% Created : 14 Jul 2013 by Gordon Guthrie
-module(wtd).

-export([
         compile/0,
         compile/1,
         get_behaviours/1,
         get_behavours/1,
         get_exports/1,
         is_exported/4,
         has_behaviour/3,
         has_behavour/3
        ]).

%% export for the test suite
-export([
         default_clef_TEST/0
        ]).

-record(annotations, {
          filename       = "",
          exports        = [],
          wtd_exports    = [],
          behaviour      = none,
          wtd_behaviours = [],
          errors         = [],
          valid          = false
         }).

-record(mission, {
          name,
          exports    = [],
          behaviours = []
         }).

-record(export, {
          modulename,
          fns        = []
         }).

-record(behaviour, {
          modulename,
          behaviour
         }).

compile(Dir) ->
    Files = filelib:wildcard(Dir ++ "/*.erl"),
    compile2(Files).

compile() ->
    Dir = get_root_dir(),
    Files = lists:merge([
                         filelib:wildcard(Dir ++ "/../../apps/*/src/*.erl"),
                         filelib:wildcard(Dir ++ "/../../deps/*/src/*.erl")
                        ]),
    compile2(Files).

compile2(Files) ->

    ok = do_housekeeping(),

    Annotations = get_annotations(Files),
    Records     = make_records(Annotations, []),
    NewRecords  = validate(Records, []),
    [pretty_print("after validation", X) || X <- NewRecords],

    case get_errors(NewRecords) of
        []   -> Data = combine(NewRecords, []),
                Dir  = get_root_dir(),
                ok   = write_crunk(Data, Dir);
        Errs -> Errs
    end.

get_errors(Records) ->
     lists:flatten([X#annotations.errors || X <- Records]).

combine([], Acc) ->
    lists:reverse(Acc);
combine([H | T], Acc) ->
    #annotations{filename       = FN,
                 wtd_exports    = WE,
                 wtd_behaviours = WBv,
                 behaviour      = BV} = H,
    NewAcc  = add_exports(WE, FN, Acc),
    io:format("Addinb behaviours to ~p~n", [NewAcc]),
    io:format("WBv is ~p~nFN is ~p~nBV is ~p~n", [WBv, FN, BV]),
    NewAcc2 = add_behaviours(WBv, FN, BV, NewAcc),
    combine(T, NewAcc2).

add_exports([], _FileName, Acc) ->
    Acc;
add_exports([{{Name, Fns}, _} | T], FileNm, Acc) ->
    NewE = #export{modulename = FileNm, fns = Fns},
    M2 = case lists:keyfind(Name, 2, Acc) of
             #mission{} = M -> #mission{exports = E} = M,
                               Es = maybe_merge(NewE, E),
                               M#mission{exports = Es};
             false          -> #mission{name = Name, exports = [NewE]}
         end,
    NewAcc = lists:keystore(Name, 2, Acc, M2),
    add_exports(T, FileNm, NewAcc).

add_behaviours([], _FileName, _Behaviour, Acc) ->
    Acc;
add_behaviours([{Name, _} | T], FileNm, Behaviour, Acc) ->
    Behav = case Behaviour of
                []              -> none;
                {supervisor, _} -> supervisor;
                {gen_server, _} -> gen_server;
                {gen_event,  _} -> gen_event;
                {gen_fsm,    _} -> gen_fsm;
                {_,          _} -> none
            end,
    NewB = #behaviour{modulename = FileNm, behaviour = Behav},
    NewAcc = case Behav of
                 none ->
                     Acc;
                 _ ->
                     M2 = case lists:keyfind(Name, 2, Acc) of
                              #mission{} = M ->
                                  #mission{behaviours = B} = M,
                                  M#mission{behaviours = [NewB | B]};
                              false ->
                                  #mission{name = Name, behaviours = [NewB]}
                          end,
                     lists:keystore(Name, 2, Acc, M2)
             end,
    add_behaviours(T, FileNm, Behaviour, NewAcc).

maybe_merge(#export{modulename = MN, fns = Fns} = Exp, Existing) ->
    E2 = case lists:keyfind(MN, 2, Existing) of
             #export{fns = OldFns} = E ->
                 NewFns = lists:merge(lists:sort(Fns), lists:sort(OldFns)),
                 E#export{fns = NewFns};
             false ->
                 Exp
         end,
    lists:keystore(MN, 2, Existing, E2).

make_records([], Acc) ->
    lists:reverse(Acc);
make_records([{File, H, []} | T], Acc) ->
    NewAcc = make_r2(H, #annotations{filename = strip(File), valid = true}),
    make_records(T, [NewAcc | Acc]);
make_records([{File, H, Errors} | T], Acc) ->
    NewAcc = make_r2(H, #annotations{filename = strip(File), errors = Errors}),
    make_records(T, [NewAcc | Acc]).

make_r2([], Rec) ->
    Rec;
make_r2([{export, E, N} | T], #annotations{exports = Es} = Rec) ->
    NewRec = Rec#annotations{exports = [{E, N} | Es]},
    make_r2(T, NewRec);
make_r2([{wtd_export, WE, N} | T], #annotations{wtd_exports = WEs} = Rec) ->
    NewRec = Rec#annotations{wtd_exports = [{WE, N} | WEs]},
    make_r2(T, NewRec);
make_r2([{behaviour, B, N} | T], Rec) ->
    NewRec = Rec#annotations{behaviour = {B, N}},
    make_r2(T, NewRec);
make_r2([{wtd_behaviour, WB, N} | T], Rec) ->
    #annotations{wtd_behaviours = WBs} = Rec,
    NewRec = Rec#annotations{wtd_behaviours = [{WB, N} | WBs]},
    make_r2(T, NewRec).

validate([], Acc) ->
    lists:reverse(Acc);
validate([#annotations{valid = false} = Rec | T], Acc) ->
    validate(T, [Rec | Acc]);
validate([H | T], Acc) ->
    #annotations{exports = E, wtd_exports = WE, errors = Errs} = H,
    %% FIXME - need to validate that the WTD Exports are valid (ie atom/integer)
    %% then extract them and validate the list of exports against normal exports
    NewRec = case validate_exports(WE, E) of
                 []      -> H;
                 ErrList -> NewErrs = lists:flatten([ErrList | Errs]),
                            H#annotations{errors = NewErrs, valid = false}
             end,
    validate(T, [NewRec | Acc]).

validate_exports(WTDExports, Exports) ->
    case validate_wtd_exps(WTDExports, []) of
        []  -> FnAndArities = extract_fns(WTDExports, []),
               Exps = lists:sort(lists:flatten([X || {X, _LineNo} <- Exports])),
               validate_wtd_vs_normal_exp(FnAndArities, Exps, []);
        Err -> Err
    end.

extract_fns([], Acc) -> lists:merge(Acc);
extract_fns([{{_Mission, List}, LineNo} | T], Acc) ->
    L = lists:sort([{X, LineNo} || X <- List]),
    extract_fns(T, [L | Acc]).

validate_wtd_exps([], Acc) ->
    lists:reverse(Acc);
validate_wtd_exps([{{_Mission, List}, LineNo} | T], Errs) when is_list(List) ->
    NewErrs = validate_fns_and_arities(List, LineNo, Errs),
    validate_wtd_exps(T, NewErrs);
validate_wtd_exps([{H, LineNo}| T], Errs) ->
    Err = {invalid_wtd_export, {H, LineNo}},
    validate_wtd_exps(T, [Err | Errs]).

validate_fns_and_arities([], _LineNo, Errs) ->
    lists:reverse(Errs);
validate_fns_and_arities([{Atom, Int} | T], LineNo, Errs)
when is_atom(Atom)   andalso
     is_integer(Int) andalso
     Int >= 0 ->
    validate_fns_and_arities(T, LineNo, Errs);
validate_fns_and_arities([H | T], LineNo, Errs) ->
    Err = {invalid_fn_and_arity, {H, LineNo}},
    validate_fns_and_arities(T, LineNo, [Err | Errs]).

validate_wtd_vs_normal_exp([], _, Errs) ->
    Errs;
validate_wtd_vs_normal_exp([{H, LineNo} | T], [], Errs) ->
    Error = {function_not_exported, {H, LineNo}},
    validate_wtd_vs_normal_exp(T, [], [Error | Errs]);
validate_wtd_vs_normal_exp([{H, _} | T1], [H | T2], Errs) ->
    validate_wtd_vs_normal_exp(T1, T2, Errs);
validate_wtd_vs_normal_exp(L, [_H | T], Errs) ->
    validate_wtd_vs_normal_exp(L, T, Errs).

get_annotations(Files) ->
    SyntaxFiles = [{X, compile_to_ast(X)} || X <- Files],
    get_a2(SyntaxFiles, []).

get_a2([], Acc) ->
    lists:reverse(Acc);
get_a2([{_, {error, File}} | T], Acc) ->
    get_a2(T, [{File, [], [{file_wont_compile, File}]} | Acc]);
get_a2([{File, {ok, [], Syn}} | T], Acc) ->
    NewAcc = get_a3(Syn, []),
    get_a2(T, [{File, NewAcc, []} | Acc]).

get_a3([], Acc) ->
    lists:reverse(Acc);
get_a3([{attribute, N, behaviour, Behaviour} | T],  Acc) ->
    get_a3(T, [{behaviour, Behaviour, N} | Acc]);
get_a3([{attribute, N, behavour, Behaviour} | T],  Acc) ->
    get_a3(T, [{behaviour, Behaviour, N} | Acc]);
get_a3([{attribute, N, wtd_behaviour, Behaviour} | T],  Acc) ->
    get_a3(T, [{wtd_behaviour, Behaviour, N} | Acc]);
get_a3([{attribute, N, wtd_behavour, Behaviour} | T],  Acc) ->
    get_a3(T, [{wtd_behaviour, Behaviour, N} | Acc]);
get_a3([{attribute, N, export, List} | T],  Acc) ->
    get_a3(T, [{export, List, N} | Acc]);
get_a3([{attribute, N, wtd_export, Struct} | T],  Acc) ->
    get_a3(T, [{wtd_export, Struct, N} | Acc]);
get_a3([_H | T],  Acc) ->
    get_a3(T, Acc).

maybe_create_clefs(Dir) ->
    Path = Dir ++ "/cbin/",
    Files = [Path ++ "outbound.clef", Path ++ "inbound.clef"],
    [ok = make_if_doesnt_exist(X) || X <- Files],
    ok.

make_if_doesnt_exist(FileName) ->
    ok = filelib:ensure_dir(FileName),
    case filelib:is_file(FileName) of
        true  -> ok;
        false -> Hdr = default_clef(),
                 ok = file:write_file(FileName, Hdr)
    end.

default_clef_TEST() ->
    lists:flatten(default_clef()).

default_clef() ->
    io_lib:format(get_erlang_hdr() ++ "~n{"
                  ++ "\"master@erlangwtd.com\","
                  ++ "\"global\","
                  ++ "\"submission\"}.~n", []).

get_erlang_hdr() ->
    "%% -*- erlang -*-".

write_crunk([], _Dir) ->
    ok;
write_crunk([#mission{} = M | T], Dir) ->
    #mission{name = NM, exports = EXP, behaviours = BHV} = M,
    FileName = Dir ++ "/cbin/" ++ atom_to_list(NM) ++ ".crunk",
    ok = filelib:ensure_dir(FileName),
    Hdr = io_lib:format(get_erlang_hdr() ++ "~n", []),
    Terms = [
             {exports,    EXP},
             {behaviours, BHV}
            ],
    Body = io_lib:format("~p.~n", [Terms]),
    Terms2 = lists:flatten([Hdr | Body]),
    ok = file:write_file(FileName, Terms2),
    write_crunk(T, Dir).

compile_to_ast(File) -> case compile:file(File, [to_pp, binary]) of
                            {ok, _, _} = Syn -> Syn;
                            error            -> {error, File}
                        end.

strip(File) ->
    [H1 | _T1] = lists:reverse(string:tokens(File, "/")),
    [H2 | _T2] = string:tokens(H1, "."),
    list_to_atom(H2).

has_behavour(Mission, Module, Behaviour) ->
    has_behaviour(Mission, Module, Behaviour).

has_behaviour(Mission, Module, Behaviour) ->
    {behaviours, Bhvs} = get_behaviours(Mission),
    case lists:keyfind(Module, 2, Bhvs) of
        false                          -> false;
        {behaviour, Module, Behaviour} -> true;
        {behaviour, Module, _}         -> false
    end.

is_exported(Export, Module, Function, Arity) when is_atom(Export)   andalso
                                                  is_atom(Module)   andalso
                                                  is_atom(Function) andalso
                                                  is_integer(Arity) ->
    {exports, Exp} = get_exports(Export),
    case lists:keyfind(Module, 2, Exp) of
        false                 -> false;
        {export, Module, Fns} -> is_exp2(Fns, Function, Arity)
    end.

is_exp2([], _, _)                                  -> false;
is_exp2([{Function, Arity} | _T], Function, Arity) -> true;
is_exp2([_H | T],                 Function, Arity) -> is_exp2(T, Function, Arity).

get_behavours(Name) -> get_behaviours(Name).

get_behaviours(Name) when is_atom(Name) ->
    {crunk, Crunk} = get_crunk(Name),
    {behaviours, _Bhvs} = lists:keyfind(behaviours, 1, Crunk).

get_exports(Name) when is_atom(Name) ->
    {crunk, Crunk} = get_crunk(Name),
    {exports, _Exp} = lists:keyfind(exports, 1, Crunk).

get_crunk(Name) when is_atom(Name) ->
    Dir = get_root_dir() ++ "cbin/",
    {ok, [Crunk]} = file:consult(Dir ++ atom_to_list(Name) ++ ".crunk"),
    {crunk, Crunk}.

pretty_print(Msg, Rec) when is_list(Msg) and is_record(Rec, annotations) ->
    #annotations{
       filename       = FN,
       exports        = Exp,
       wtd_exports    = WTD_Exp,
       behaviour      = B,
       wtd_behaviours = WTD_Bs,
       errors         = Errs,
       valid          = V} = Rec,
    io:format("~p~n"
              ++ "- File Name      : ~p~n"
              ++ "- Exports        : ~p~n"
              ++ "- WTD Exports    : ~p~n"
              ++ "- Behaviour      : ~p~n"
              ++ "- WTD Behaviours : ~p~n"
              ++ "- Errors         : ~p~n"
              ++ "- Valid?         : ~p~n",
              [Msg, FN, Exp, WTD_Exp, B, WTD_Bs, Errs, V]).

clear_crunk() ->
    Dir = get_root_dir(),
    Files = filelib:wildcard(Dir ++ "/cbin/*.crunk"),
    [ok = file:delete(X) || X <- Files],
    ok.

do_housekeeping() ->
    Dir = get_root_dir(),
    ok  = clear_crunk(),
    ok  = maybe_create_clefs(Dir).

get_root_dir() -> filename:dirname(code:where_is_file("erlang_wtd.app")) ++ "/../".

