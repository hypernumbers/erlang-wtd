-module(install_plugin).

-compile(export_all).

% pre_install_history(_, _) ->
%    ok.

install_history(_Config, _AppFile) ->
    case os:getenv("USER") of
        "root" -> install();
        _      -> io:format("erlang-history can only be installed as root...~n")
    end.

% post_install_history(_, _) ->
%    ok.

install() ->
    {ok, CWD} = file:get_cwd(),
    CMD = CWD ++ "/deps/erlang-history/install.escript",
    Return = os:cmd(CMD),
    io:format("OS cmd return ~p~n", [Return]),
    ok.
