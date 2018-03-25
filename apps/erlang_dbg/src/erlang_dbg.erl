-module(erlang_dbg).

-export([main/1]).

-define(DEFAULT_PORT, 9000).

main(Args) ->
    case getopt:parse(cli_options(), Args) of
        {ok, {Opts, _Other}} ->
            OptsMap = maps:from_list(proplists:unfold(Opts)),
            run(Args, OptsMap);
        _Err ->
            io:format("Error: ~p~n", [_Err]),
            getopt:usage(cli_options(), "dbg_server")
    end.

cli_options() ->
    [
     {help,     $h,        "help",      undefined, "Show this help"},
     {port,     $p,        "port",      integer,   "DBG server port"},
     {verbose,  $v,        "verbose",   integer,   "Verbosity level"},
     {path_add,  $a,        "pa",       string,    "Add my own paths first"},
     {path_addz, $z,        "pz",       string,    "Add my own paths last"},
     {start,     $s,        "start",        string,    "Start own processes"},
     {eval,      $e,        "eval",     string,    "Eval inline script"},
     {args_file, undefined, "args_file",string,    "Read arguments from file"}
    ].

run(Args, MappedOpts) ->
    Verbose = maps:get(verbose, MappedOpts, 0),
    case MappedOpts of
        #{help := _} ->
            getopt:usage(cli_options(), "dbg_server"),
            erlang:halt(0);
        _ ->
            start_server(Args, MappedOpts, [])
    end.

start_server(Args, MappedOpts, Config) ->
    Port = maps:get(port, MappedOpts, proplists:get_value(port, Config, ?DEFAULT_PORT)),
    ok = application:load(dbg_server),
    ok = application:set_env(dbg_server, port, Port),
    ok = application:set_env(dbg_server, implementor, erlang_dbg_implementor),

    case application:ensure_all_started(dbg_server, permanent) of
        {ok, _R} ->
            dbg_cmd_line:execute_args(Args),
            receive stop -> ok end,
            ok;
        _Err ->
            io:format("Startup error: ~p~n", [_Err]),
            ok
    end.