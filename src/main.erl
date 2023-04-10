%%%----FILE main.erl----

%%%

-module(main).

%-export([initialize/0, initialize/1]).
-export([run/0]).

%-include("comms.hrl").
-include("account.hrl").
-include("server.hrl").

-include_lib("kernel/include/logger.hrl").

initialize() ->
    case file:consult("data/servers") of
        {ok, Servers} ->
            lists:foreach(fun(Srv) -> initialize(Srv) end, Servers);
        {error, enoent} ->
            initialize([server:new(no_id)]);
        {error, Reason} ->
            error({?FUNCTION_NAME, Reason})
    end.

initialize(Server) ->
    Pid = spawn_link(fun() -> server:actor(Server) end),
    catch unregister(Server#server.id),
    register(Server#server.id, Pid),
    Server.

run() ->
    ?LOG_DEBUG("Main loop started"),
    catch unregister(?MAIN),
    Pid = spawn(fun() -> actor() end),
    register(?MAIN, Pid),
    ?LOG_DEBUG("Main loop started").

actor() ->
    ?LOG_DEBUG("~w actor active and listening", [?MAIN]),
    receive
        {Sender, initialize, Server} ->
            ?LOG_DEBUG("Received initialize request: ~w", [Server]),
            Sender ! {?MAIN, initialized, initialize(Server)};
        {Sender, create, Server} ->
            Sender ! error("unimplemented");
        {Sender, halt, Server} ->
            Sender ! error("unimplemented");
        {Sender, _, _} ->
            Sender ! error("unimplemented");
        _ ->
            error("unimplemented")
    end,
    actor().
