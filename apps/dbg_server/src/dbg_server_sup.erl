-module(dbg_server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    R = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    R.

init([]) ->
    Children = children(),
    %% no supervisor restarts
    {ok, {{one_for_one, 0, 1}, Children}}.

children() ->
    {ok, Port} = application:get_env(dbg_server, port),
    {ok, Implementor} = application:get_env(dbg_server, implementor),
    DbgRpc = {dbgrpc, {dbgrpc, start_link, [Port, dbg_server, dbg_client]},
        permanent, 60000, worker, [dbgrpc]},
    IdeServer = {dbg_server, {dbg_server, start_link, [Implementor]},
        permanent, 60000, worker, [lsp_server]},
%    IdeClient = {dbg_client, {dbg_client, start_link, []},
%        permanent, 60000, worker, [dbg_client]},
    [
        DbgRpc
        , IdeServer
%        , IdeClient
    ].

