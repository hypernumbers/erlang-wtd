-module(crunk).

-export([
         crunk/0
        ]).

-record(extract, {
          behaviour,
          missions           = []
         }).

crunk() ->
    Dir = code:lib_dir(wtd),
    Files = lists:merge([filelib:wildcard(Dir ++ "/../../apps/*/src/*.erl"),
                         filelib:wildcard(Dir ++ "/../../deps/*/src/*.erl")]),
    Missions = extract(Files),
    Crunk = invert(Missions, dict:new()),
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

invert([], Dict) ->
    dict:to_list(Dict);
invert([{File, #extract{behaviour = B, missions = M}} | T], Dict) ->
    NewDict = insert(M, B, File, Dict),
    invert(T, NewDict).

insert([], _B, _File, Dict) ->
    Dict;
insert([{Mission, Fns} | T], Behaviour, File, Dict) ->
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
    insert(T, Behaviour, File, NewDict).

extract(Files) ->
    SyntaxFiles = [compile(X) || X <- Files],
    extract_missions(SyntaxFiles).

extract_missions(Syntax) -> eliminate(extract_m2(Syntax, [])).

extract_m2([], Acc) ->
    lists:reverse(Acc);
extract_m2([{ok, [], Syn} | T], Acc) ->
    NewAcc = extract_m3(Syn, "", []),
    extract_m2(T, [NewAcc | Acc]).

extract_m3([], File, Acc) ->
    {strip(File), chunk(Acc)};
extract_m3([{attribute, 1, file, {File, 1}} | T], _, Acc) ->
    extract_m3(T, File, Acc);
extract_m3([{attribute, _, behaviour, wtd_server} | T], File, Acc) ->
    extract_m3(T, File, [wtd_server | Acc]);
extract_m3([{attribute, _, behaviour, wtd_fsm} | T], File, Acc) ->
    extract_m3(T, File, [wtd_fsm | Acc]);
extract_m3([{attribute, _, behaviour, wtd_event} | T], File, Acc) ->
    extract_m3(T, File, [wtd_event | Acc]);
extract_m3([{attribute, _, behaviour, wtd_supervisor} | T], File, Acc) ->
    extract_m3(T, File, [wtd_supervisor | Acc]);
extract_m3([{attribute, _, export, List} | T], File, Acc) ->
    extract_m3(T, File, [{export, List} | Acc]);
extract_m3([{attribute, _, mission, Struct} | T], File, Acc) ->
    extract_m3(T, File, [{mission, Struct} | Acc]);
extract_m3([_H | T], File, Acc) ->
    extract_m3(T, File, Acc).

eliminate(List) ->
    el2(List, []).

el2([], Acc) ->
    lists:reverse(Acc);
el2([{_, #extract{behaviour = undefined, missions = []}} | T], Acc) ->
    el2(T, Acc);
el2([H | T], Acc) ->
    el2(T, [H | Acc]).

compile(File) -> compile:file(File, [to_pp, binary]).

strip(File) ->
    [H1 | _T1] = lists:reverse(string:tokens(File, "/")),
    [H2 | _T2] = string:tokens(H1, "."),
    list_to_existing_atom(H2).

chunk(List) ->
    make_record(List, [], [], []).

make_record([], A1, A2, A3) ->
    mk(A1, lists:merge(A2), consolidate(A3));
make_record([Type | T], A1, A2, A3)
  when Type == wtd_server
       orelse Type == wtd_fsm
       orelse Type == wtd_event
       orelse Type == wtd_supervisor ->
    make_record(T, [Type | A1], A2, A3);
make_record([{export, Exports} | T], A1, A2, A3) ->
    make_record(T, A1, [Exports | A2], A3);
make_record([{mission, {_, _} = Missions} | T], A1, A2, A3) ->
    make_record(T, A1, A2, [Missions | A3]).

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
mk([], Exports, Missions) ->
    case mk2(flatten(Missions), lists:sort(Exports)) of
        true  -> #extract{missions = Missions};
        false -> exit("invalid missions...")
    end;
mk([Behaviour], Exports, Missions) ->
    case mk2(flatten(Missions), lists:sort(Exports)) of
        true  -> #extract{behaviour = Behaviour, missions = Missions};
        false -> exit("invalid missions...")
    end.

mk2([], _) ->
    true;
mk2([H | T1], [H | T2]) ->
    mk2(T1, T2).

flatten(List) ->
    lists:usort(lists:flatten([X || {_, X} <- List])).
