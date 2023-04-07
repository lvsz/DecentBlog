%%%----FILE server.hrl----

%%%

-ifndef(SERVER_HRL_).
-define(SERVER_HRL_, 1).
%%--------------------------------------------------------------------------

-record(server, {
    id :: server:id(),
    users = account:new_db() :: account:db(),
    active = dict:new() :: dict:dict(pid(), account:id()),
    channel = comms:new_channel(server) :: comms:channel()
}).

%%--------------------------------------------------------------------------
-endif.
