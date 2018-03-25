-module(erlang_int_listener).

-export([start/0]).


start() ->
    int:start(),
    %observer:start(),
    spawn(fun () -> init_subscribe() end).

init_subscribe() ->    
    register(?MODULE, self()),
    int:subscribe(),
    loop_subscribe(0).

loop_subscribe(State) ->
    receive
		{int, M} ->
            gen_server:call(dbg_server, {'decode_debugger_message', {int, M}}),
			loop_subscribe(State);
		M ->
			io:format("receive : ~p~n", [M]),
			loop_subscribe(State)
	end.
 

