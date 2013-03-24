-module(crunk).

-export([
         crunk/0
        ]).

-record(extract, {
          behaviour,
          missions      = [],
          documentation = []
         }).

-record(doco,
        {
          module,
          name,
          arity,
          doc  = [],
          spec = [],
          fn   = []
        }).

crunk() ->
    Dir = code:lib_dir(wtd),
    % Files = lists:merge([filelib:wildcard(Dir ++ "/../../apps/*/src/*.erl"),
    %         filelib:wildcard(Dir ++ "/../../deps/*/src/*.erl")]),
    Files = ["/home/gordon/erlang-wtd/deps/wtd/src/junk.erl"],
    Missions = extract_missions(Files),
    io:format("Missions is ~p~n", [Missions]),
    Crunk = invert_data_structure(Missions, dict:new()),
    ok = write_crunk(Crunk, Dir),
    ok = create_clefs(Dir).

create_clefs(Dir) ->
    Path = Dir ++ "/../../cbin/",
    Files = [Path ++ "outbound.clef", Path ++ "inbound.clef"],
    [ok = make_if_doesnt_exist(X) || X <- Files],
    ok.

make_if_doesnt_exist(FileName) ->
    case filelib:is_file(FileName) of
        true  -> ok;
        false -> Hdr = io_lib:format("%% -*- erlang -*-~n{"
                        ++ "\"master@erlangwtd.com\","
                        ++ "\"global\","
                        ++ "\"submission\"}.~n", []),
                 ok = file:write_file(FileName, Hdr)
    end.

write_crunk([], _Dir) ->
    ok;
write_crunk([{Mission, Crunk} | T], Dir) ->
    FileName = Dir ++ "/../../cbin/" ++ atom_to_list(Mission) ++ ".crunk",
    ok = filelib:ensure_dir(FileName),
    Hdr = io_lib:format("%% -*- erlang -*-~n", []),
    Terms = lists:flatten([Hdr | [io_lib:format("~p.~n", [X]) || X <- Crunk]]),
    ok = file:write_file(FileName, Terms),
    write_crunk(T, Dir).

invert_data_structure([], Dict) ->
    dict:to_list(Dict);
invert_data_structure([{File, #extract{behaviour = B, missions = M}} | T],
                     Dict) ->
    NewDict = insert_into_dict(M, B, File, Dict),
    invert_data_structure(T, NewDict).

insert_into_dict([], _B, _File, Dict) ->
    Dict;
insert_into_dict([{Mission, Fns} | T], Behaviour, File, Dict) ->
    Type = case Behaviour of
               undefined -> module;
               _         -> Behaviour
           end,
    NewVals = case dict:is_key(Mission, Dict) of
                  true  -> {ok, Vals} = dict:find(Mission, Dict),
                           [{File, Fns, Type} | Vals];
                  false -> [{File, Fns, Type}]
              end,
    NewDict = dict:store(Mission, NewVals, Dict),
    insert_into_dict(T, Behaviour, File, NewDict).

extract_missions(Files) ->
    List = [{edoc:read_source(X), edoc:read_comments(X)} || X <- Files],
    SyntaxFiles = merge_compile_and_documentation(List),
    extract_m2(SyntaxFiles).

extract_m2(Syntax) -> eliminate_undefined_missions(extract_m3(Syntax, [])).

extract_m3([], Acc) ->
    lists:reverse(Acc);
extract_m3([H | T], Acc) ->
    NewAcc = e_m4(H, "", "", []),
    extract_m3(T, [NewAcc | Acc]).

e_m4([], File, Doc, Acc) ->
    {{File, Doc}, chunk(Acc)};
e_m4([{docu, 1, file, Doc} | T], File, _, Acc) ->
    e_m4(T, File, Doc, Acc);
e_m4([{attribute, _, module, File} | T], _, Doc, Acc) ->
    e_m4(T, File, Doc, Acc);
e_m4([{attribute, _, behaviour, wtd_server} | T], File, Doc, Acc) ->
    e_m4(T, File, Doc, [wtd_server | Acc]);
e_m4([{attribute, _, behaviour, wtd_fsm} | T], File, Doc, Acc) ->
    e_m4(T, File, Doc, [wtd_fsm | Acc]);
e_m4([{attribute, _, behaviour, wtd_event} | T], File, Doc, Acc) ->
    e_m4(T, File, Doc, [wtd_event | Acc]);
e_m4([{attribute, _, behaviour, wtd_supervisor} | T], File, Doc, Acc) ->
    e_m4(T, File, Doc, [wtd_supervisor | Acc]);
e_m4([{attribute, _, export, List} | T], File, Doc, Acc) ->
    e_m4(T, File, Doc, [{export, List} | Acc]);
e_m4([{attribute, _, mission, Struct} | T], File, Doc, Acc) ->
    e_m4(T, File, Doc, [{mission, Struct} | Acc]);
%% a docu line can immediately a preceed a typespec or a function
e_m4([{docu, _, D}, {attribute, _, spec, Sp} | T], File, Doc, Acc) ->
    {{Name, Ar}, _} = Sp,
    NewAcc = #doco{module = File, name = Name, arity = Ar, doc = D, spec = Sp},
    e_m4(T, File, Doc, [NewAcc | Acc]);
%% a docu line can immediately a preceed a typespec or a function
e_m4([{docu, _, D}, {function, _, Nm, Ar, Fn} | T], File, Doc, Acc) ->
    NewAcc = #doco{module = File, name = Nm, arity = Ar, doc = D, fn = Fn},
    e_m4(T, File, Doc, [NewAcc | Acc]);
e_m4([_H | T], File, Doc, Acc) ->
    e_m4(T, File, Doc, Acc).

eliminate_undefined_missions(List) ->
    el2(List, []).

el2([], Acc) ->
    lists:reverse(Acc);
el2([{_, #extract{behaviour = undefined, missions = []}} | T], Acc) ->
    el2(T, Acc);
el2([H | T], Acc) ->
    el2(T, [H | Acc]).

merge_compile_and_documentation(List) -> merge2(List, []).

merge2([], Acc) ->
    lists:reverse(Acc);
merge2([{Syntax, Docs} | T], Acc) ->
    NewAcc = merge3(Syntax, Docs, []),
    merge2(T, [NewAcc | Acc]).

merge3([], [], Acc) ->
    lists:reverse(Acc);
merge3([], [Doc | T], Acc) ->
    merge3([], T, [Doc | Acc]);
merge3([Syn | T], [], Acc) ->
    merge3(T, [], [Syn | Acc]);
merge3([{attribute, N, _, _} = Syn | T1], [{M, _, _, _} = Doc | T2], Acc)
  when N < M ->
    merge3(T1, [Doc | T2], [Syn | Acc]);
merge3([{_, N, _, _, _} = Syn | T1], [{M, _, _, _} = Doc | T2], Acc)
  when N < M ->
    merge3(T1, [Doc | T2], [Syn | Acc]);
merge3([{attribute, N, _, _} = Syn | T1], [{M, _, _, List} | T2], Acc)
  when N > M ->
    NewDoc = {docu, M, List},
    merge3([Syn | T1], T2, [NewDoc | Acc]);
merge3([{_, N, _, _, _} = Syn | T1], [{M, _, _, List} | T2], Acc)
  when N > M ->
    NewDoc = {docu, M, List},
    merge3([Syn | T1], T2, [NewDoc | Acc]).

chunk(List) ->
    make_record(List, [], [], [], []).

make_record([], A1, A2, A3, A4) ->
    mk(A1, lists:merge(A2), consolidate(A3), A4);
make_record([Type | T], A1, A2, A3, A4)
  when Type == wtd_server
       orelse Type == wtd_fsm
       orelse Type == wtd_event
       orelse Type == wtd_supervisor ->
    make_record(T, [Type | A1], A2, A3, A4);
make_record([{export, Exports} | T], A1, A2, A3, A4) ->
    make_record(T, A1, [Exports | A2], A3, A4);
make_record([{mission, {_, _} = Missions} | T], A1, A2, A3, A4) ->
    make_record(T, A1, A2, [Missions | A3], A4);
make_record([#doco{} = D | T], A1, A2, A3, A4) ->
    make_record(T, A1, A2, A3, [D | A4]).

consolidate(List) ->
    con2(lists:sort(List), dict:new()).

con2([], Dict) ->
    dict:to_list(Dict);
con2([{Key, Val} | T], Dict) ->
    NewDict = case dict:is_key(Key, Dict) of
                  true  -> NewList = lists:merge([Val | dict:fetch(Key, Dict)]),
                           dict:store(Key, NewList, Dict);
                  false -> dict:store(Key, [Val], Dict)
              end,
    con2(T, NewDict).

% there should only be one behaviour per module
mk([], Exports, Missions, Documentation) ->
    case mk2(flatten(Missions), lists:sort(Exports)) of
        true  -> #extract{missions = Missions, documentation = Documentation};
        false -> exit("invalid missions...")
    end;
mk([Behaviour], Exports, Missions, Documentation) ->
    case mk2(flatten(Missions), lists:sort(Exports)) of
        true  -> #extract{behaviour = Behaviour, missions = Missions,
                          documentation = Documentation};
        false -> exit("invalid missions...")
    end.

mk2([], _) ->
    true;
mk2([H | T1], [H | T2]) ->
    mk2(T1, T2).

flatten(List) ->
    lists:usort(lists:flatten([X || {_, X} <- List])).
