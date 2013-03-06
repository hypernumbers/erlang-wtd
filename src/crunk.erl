-module(crunk).

-export([
         crunk/0
        ]).

-record(crunk, {
          behaviours,
          functions
          }).

-record(extract, {
          behaviour,
          missions  = []
         }).

crunk() ->
    Files = filelib:wildcard("src/*.erl"),
    Missions = extract(Files),
    io:format("Missions is ~p~n", [Missions]),
    Inv = invert(Missions, dict:new()),
    io:format("Inv is ~p~n", [Inv]).

invert([], Acc) ->
    Acc;
invert([{_File, #extract{behaviour = _B, missions = _M}} | T], Acc) ->
    invert(T, Acc).

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
    {strip(File), chunck(Acc)};
extract_m3([{attribute, 1, file, {File, 1}} | T], _, Acc) ->
    extract_m3(T, File, Acc);
extract_m3([{attribute, _, behaviour, {minion, M}} | T], File, Acc) ->
    extract_m3(T, File, [{minion, M} | Acc]);
extract_m3([{attribute, _, behaviour, {evilplan, M}} | T], File, Acc) ->
    extract_m3(T, File, [{evilplan, M} | Acc]);
extract_m3([{attribute, _, behaviour, {hairyarsedpict, M}} | T], File, Acc) ->
    extract_m3(T, File, [{hairyarsedpict, M} | Acc]);
extract_m3([{attribute, _, behaviour, {bossman, M}} | T], File, Acc) ->
    extract_m3(T, File, [{bossman, M} | Acc]);
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
    H2.

chunck(List) ->
    make_record(List, [], [], []).

make_record([], A1, A2, A3) ->
    mk(A1, lists:merge(A2), consolidate(A3));
make_record([{Type, _} = Ty | T], A1, A2, A3)
  when Type == minion
       orelse Type == evilpaln
       orelse Type == hairyarsedpick
       orelse Type == bossman ->
    make_record(T, [Ty | A1], A2, A3);
make_record([{export, Exports} | T], A1, A2, A3) ->
    make_record(T, A1, [Exports | A2], A3);
make_record([{mission, Missions} | T], A1, A2, A3) ->
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
