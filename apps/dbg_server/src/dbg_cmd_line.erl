-module(dbg_cmd_line).

-export([execute_args/1, parse_and_run_arguments/1, parse_and_run_specific_arguments/2]).


parse_and_run_arguments(Args) when is_binary(Args) ->
    parse_and_run_arguments(binary_to_list(Args));

parse_and_run_arguments(Args) ->
    ParsedArgs = parse_arguments(Args),
    io:format("parsed_args : ~p~n", [ParsedArgs]),
    execute_args(ParsedArgs).


parse_and_run_specific_arguments(Args, Fun) when is_binary(Args) ->
    parse_and_run_specific_arguments(binary_to_list(Args), Fun);

parse_and_run_specific_arguments(Args, Fun) ->
    ParsedArgs = parse_arguments(Args),
    execute_args(ParsedArgs, Fun).

parse_arguments(_Args) ->
    {ok, R} = re:compile("(?:(?P<A>([\'\"]))(?P<V>.+)(?P=A))|(?P<V1>[^ ]+)", [extended]),
    case re:run(_Args, R, [global,{capture, [0],list}]) of
    {match, Res} -> lists:map(fun([X]) -> X end, Res);
    _ -> []
    end.

%see https://github.com/erlang/otp/blob/c881fcf64283223eb88d539b1e88d16a29d08cd9/erts/preloaded/src/init.erl
% to parse args_file
% inspired from init.erl 

execute_args(Args) ->
    execute_args(Args, fun(_) -> true end).

execute_args(Args, Fun) ->
    ParsedArgs = parse_args(Args),
    FilteredArgs = lists:filter(Fun, ParsedArgs),
    Start = map(fun prepare_run_args/1, FilteredArgs),
    start_em(Start).

prepare_run_args({eval, [Expr]}) ->
    {eval,Expr};
prepare_run_args({path_pa, Paths}) ->
    {path_pa, Paths};
prepare_run_args({path_pz, Paths}) ->
    {path_pz, Paths};
prepare_run_args({apply, [M,F|Args]}) ->
    [b2a(M), b2a(F) | bs2as(Args)];
prepare_run_args({apply, [M]}) ->
    [b2a(M)].

parse_args(Args) ->
    parse_args(Args, []).

parse_args([B|Bs], Ss) ->
    case get_switch(B) of
	start_arg ->
	    {S,Rest} = get_args(Bs, []),
	    parse_args(Rest, [{apply, S}|Ss]);
	path_pa ->
	    {S,Rest} = get_args(Bs, []),
	    parse_args(Rest, [{path_pa, S}|Ss]);
	path_pz ->
	    {S,Rest} = get_args(Bs, []),
	    parse_args(Rest, [{path_pz, S}|Ss]);        
	eval_arg ->
	    {Expr,Rest} = get_args(Bs, []),
	    parse_args(Rest, [{eval, Expr}|Ss]);
    args_file ->
	    {File,Rest} = get_args(Bs, []),
	    parse_args(Rest, [extract_args_from_file(File)|Ss]);        
	_ ->
	    parse_args(Bs, Ss)
    end;
parse_args([], Start) ->
    reverse(Start).

extract_args_from_file(_File) ->
    %read file then split into Args, then parse_args(Args)
    [].

get_switch("--start") -> start_arg;
get_switch("-s") -> start_arg;
get_switch("--eval") -> eval_arg;
get_switch("-eval") -> eval_arg;
get_switch("-e") -> eval_arg;
get_switch("-a") -> path_pa;
get_switch("-z") -> path_pz;
get_switch("--pa") -> path_pa;
get_switch("-pa") -> path_pa;
get_switch("--pz") -> path_pz;
get_switch("-pz") -> path_pz;
get_switch("--args_file") -> args_file;
get_switch(_) -> arg.

get_args([B|Bs], As) ->
    case get_switch(B) of
	start_arg -> {reverse(As), [B|Bs]};
	path_pa -> {reverse(As), [B|Bs]};
	path_pz -> {reverse(As), [B|Bs]};
	eval_arg -> {reverse(As), [B|Bs]};
	args_file -> {reverse(As), [B|Bs]};
	arg ->
	    get_args(Bs, [B|As])
    end;
get_args([], As) -> {reverse(As),[]}.

start_em([S|Tail]) ->
    start_it(S),
    start_em(Tail);
start_em([]) -> ok.

start_it([]) ->
    ok;
start_it({eval,Bin}) ->
    Str = b2s(Bin),
    {ok,Ts,_} = erl_scan:string(Str),
    Ts1 = case reverse(Ts) of
	      [{dot,_}|_] -> Ts;
	      TsR -> reverse([{dot,erl_anno:new(1)} | TsR])
	  end,
    {ok,Expr} = erl_parse:parse_exprs(Ts1),
    {value, _Value, _Bs} = erl_eval:exprs(Expr, erl_eval:new_bindings()),
    ok;
start_it({path_pa, Paths}) ->
    Ret = code:add_pathsa(Paths),
    %io:format("add_patha ~p~n",[Ret]),
    Ret;
start_it({path_pz, Paths}) ->
    Ret = code:add_pathsz(Paths),
    %io:format("add_pathz ~p~n",[Ret]),
    Ret;
start_it([_|_]=MFA) ->
    case MFA of
	[M]        -> 
        io:format("run ~p:start()~n", [M]),
        M:start();
	[M,F]      -> 
        io:format("run M:F()~n"),
        M:F();
	[M,F|Args] -> 
        io:format("run M:F(Args)~n"),
        M:F(Args)	% Args is a list
    end.

reverse([] = L) ->
    L;
reverse([_] = L) ->
    L;
reverse([A, B]) ->
    [B, A];
reverse([A, B | L]) ->
    lists:reverse(L, [B, A]). % BIF

b2s(Bin) when is_binary(Bin) ->
    try
	unicode:characters_to_list(Bin,file:native_name_encoding())
    catch
	_:_ -> binary_to_list(Bin)
    end;
b2s(L) when is_list(L) ->
    L.

b2a(Bin) when is_binary(Bin) ->
    list_to_atom(b2s(Bin));
b2a(A) when is_list(A) ->
    list_to_atom(A);
b2a(A) when is_atom(A) ->
    A.

bs2as(L0) when is_list(L0) ->
    map(fun b2a/1, L0);
bs2as(L) ->
    L.

map(_F, []) ->
    [];
map(F, [X|Rest]) ->
    [F(X) | map(F, Rest)].