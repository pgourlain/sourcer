%%% @author Pierrick Gourlain
%%% @doc Handle VSCode Debugger Protocol messages 
%%% and handle message receive from erlang debugger 'int' (decode_debugger_message/2)

-module(erlang_dbg_implementor).


-export([
        'initialize'/1,
        'launch'/2,
        'disconnect'/3,
        'setBreakpoints'/3,
        'configurationDone'/3,
        'threads'/3,
        default_answer/1,

        'decode_debugger_message'/2
        ]).


-record(state, {
        arguments,
        cwd,
        addEbinsToCodepath
        }).

'initialize'(_InitializeParams) ->
    ConfigResponse = #{
        supportsConfigurationDoneRequest => true,
        supportsConditionalBreakpoints => false,
        supportsFunctionBreakpoints => false,
        supportsEvaluateForHovers => false,
        supportsSetVariable => false,
        supportsStepBack => false,
        exceptionBreakpointFilters => []
    },
    {ConfigResponse, #state{}}.

'disconnect'(UserState, _Params, Reporter) ->
    int:stop(),
    send_notification(<<"terminated">>, #{}), 
    Reporter({value, #{}}),
    UserState.

'launch'(UserState, Params) ->
    erlang_int_listener:start(), 
    send_notification(<<"initialized">>, #{}),   
    NewState = launch_params_to_state([arguments, cwd, addEbinsToCodepath], Params, UserState, fun update_state/3),
    set_working_dir(NewState#state.cwd),
    %TODO: add ebin dirs
    %execute only add_path
    dbg_cmd_line:parse_and_run_specific_arguments(NewState#state.arguments, fun(Switch) ->
        case Switch of
        { path_pa, _ } -> true;
        { path_pz, _ } -> true;
        _ -> false
        end
    end),
    %io:format("launch is called : with state~p~n", [UserState]),
    {#{}, NewState}.

%#{arguments =>
%      #{breakpoints => [#{line => 31}],
%        lines => [31],
%        source =>
%            #{name => <<"sample_app.erl">>,
%              path =>
%                  <<"/Users/genius/Documents/sources/erlang/sample/_build/default/lib/sample/src/sample_app.erl">>},
%        sourceModified => false},
%  command => <<"setBreakpoints">>,seq => 3,
%  type => <<"request">>}
'setBreakpoints'(UserState, Params, Reporter) ->
    %io:format("setBreakpoints is called~n"),     
    {ok, Bps} = maps:find(breakpoints, Params),
    {ok, Source} = maps:find(source, Params),
    {ok, F} = maps:find(name, Source),
    M = list_to_atom(binary_to_list(filename:rootname(F))),
    {ok, Lines} = maps:find(lines, Params),
    set_bp(M, Lines),
    %io:format("setBreakpoints done~n"), 
    Reporter({value, #{}}),
    UserState.
   

%#{command => <<"configurationDone">>,seq => 7,
%                 type => <<"request">>}

'configurationDone'(UserState, _Params, Reporter) ->
    io:format("configurationDone is called :~p~n",[UserState]), 
    Reporter({value, #{}}),
    %run all arguments
    spawn(fun() ->
        dbg_cmd_line:parse_and_run_arguments(UserState#state.arguments)
    end),
    UserState.

'threads'(UserState, _Params, Reporter) ->
    Reporter({value, #{ threads => []}}),
    UserState.

% event for vscode are describe here
% https://github.com/Microsoft/vscode-debugadapter-node/blob/master/protocol/src/debugProtocol.ts
'decode_debugger_message'({int, M}, State) ->
    case M of
    {new_process, {_Pid, {_Module, module_info, _Args}, _Status, _Other}} ->
        % ignore processes calling module_info started by debugger itself        
        ok;
    {break_options, _Data} ->
        ok;
    {interpret, Module} ->
        MB = list_to_binary(atom_to_list(Module)),
        send_notification(<<"module">>, #{ 
            reason => <<"new">>, 
            module => #{ id => MB, name => MB }
        }),        
        ok;
    {new_process, {Pid,_,_Status,_}} ->
        send_notification(<<"thread">>, #{ reason => <<"started">>, threadId => pid_to_number(Pid)}),
        ok;
    {Verb, Data} ->
        ok;

    {new_status,Pid,idle,_} ->
        ok;
    {new_status,Pid,exit,_} ->
        send_notification(<<"thread">>, #{ reason => <<"exited">>, threadId => pid_to_number(Pid)}),
        ok;
    {new_status,Pid,break,ModuleAndLine} ->
        %{new_status,<0.3.0>,break,{myapp,11}}   
        send_notification(<<"stopped">>, #{ 
            reason => <<"breakpoint">>, 
            threadId => pid_to_number(Pid)
        }),
        ok;
    {new_status,_,running,_} ->
        %{new_status,<0.3.0>,running,{}}
        ok;
    {new_status,_,waiting,_} ->
        %{new_status,<0.3.0>,waiting,{}}: no output not to pollute the console
        ok;
    _ -> 
        log("unhandled decoder for message : ~p~n", [M])    
    end,
    State.

send_notification(EventName, Body) ->
    spawn(fun () -> send_notification_after(EventName, Body) end).

send_notification_after(EventName, Body) ->
    receive
    after 100 ->
        dbgrpc:send_notification(EventName, Body)
    end.

default_answer(launch) ->
    io:format("default_answer(launch) is called"),
    [];
default_answer(completion_resolve) ->
    null;
default_answer(hover) ->
    null;
default_answer(signature_help) ->
    null;
default_answer(_Cmd) ->
    io:format("default_answer(~p) is called",[_Cmd]),
    [].

%%%%%%%%%%%%%%%%%%%%%
% private methods
%%%%%%%%%%%%%%%%%%%%%

launch_params_to_state([K|Rest], Params, State, UpdateFun) ->
    NewState = case maps:find(K, Params) of
    {ok, Value} -> UpdateFun(K, Value, State);
    _ -> State
    end,
    launch_params_to_state(Rest, Params, NewState, UpdateFun);

launch_params_to_state([], _Params, State, _UpdateFun) ->
    State.

update_state(K, Value, State) ->
    case K of 
        arguments -> State#state{arguments = Value};
        cwd -> State#state{cwd = Value};
        addEbinsToCodepath -> State#state{addEbinsToCodepath = Value}; 
        _ -> State
    end.

set_working_dir(Cwd) when is_binary(Cwd) ->
    set_working_dir(binary_to_list(Cwd));

set_working_dir(Cwd) ->
    c:cd(Cwd).

set_bp(Module, Lines) ->
    case int:all_breaks(Module) of
        [] ->
            case int:interpretable(Module) of
                true -> int:ni(Module);
                _ -> error
            end;
        _ -> ok
    end,
    lists:foreach(fun(Line) ->
        case int:break(Module, Line) of
            ok -> ok;
            Error -> io:format("Cannot set brakepoint ~p:~p by ~p~n", [Module, Line, Error])
        end
    end, Lines),
    ok. 

pid_to_number(Pid) ->
    P = string:trim(pid_to_list(Pid), both, "<>"),
    list_to_integer(binary_to_list(list_to_binary(string:replace(P, ".", "", all)))).

log(MsgFmt, Args) ->
    Msg = list_to_binary(io_lib:format(MsgFmt,Args)),
    io:format("INT: ~p~n", [Msg]).  