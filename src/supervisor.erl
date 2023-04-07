%%%----FILE supervisor.erl----

%%%

-module(supervisor).
-export([initialize/0]).
-include("comms.hrl").
-include("server.hrl").

% 1. initialize servers & channels
% 2. listen for logins
%
initialize() ->
    case file:consult("servers") of
        {ok, Servers} ->
            initialize(Servers);
        {error, enoent} ->
            initialize([dict:new()]);
        {error, Reason} ->
            error({?FUNCTION_NAME, Reason})
    end.

% a channel is any collection of posts
% each server has a private and public channel
% A server is a name, a list of users, a private channel, and a public channel
initialize([]) ->
    done;
initialize([Server | Servers]) ->
    Pid = spawn_link(fun() -> server:actor(Server) end),
    catch unregister(Server#server.id),
    register(Server#server.id, Pid),
    initialize(Servers).
