%%%----FILE main.erl----

%%%

-module(main).

-export([initialize/0, initialize/1]).

%-include("comms.hrl").
-include("account.hrl").
-include("server.hrl").

-include_lib("kernel/include/logger.hrl").

initialize() ->
    case file:consult("data/servers") of
        {ok, Servers} ->
            initialize(Servers);
        {error, enoent} ->
            initialize([server:new(no_id)]);
        {error, Reason} ->
            error({?FUNCTION_NAME, Reason})
    end.

initialize([]) ->
    done;
initialize([Server | Servers]) ->
    Pid = spawn_link(fun() -> server:actor(Server) end),
    catch unregister(Server#server.id),
    register(Server#server.id, Pid),
    initialize(Servers).
