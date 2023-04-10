%%%----FILE server.hrl----

%%%

-ifndef(SERVER_HRL_).
-define(SERVER_HRL_, 1).

%%--------------------------------------------------------------------------

-define(MAIN, '__TOP_ACTOR__').

-record(server, {
    id :: server:id(),
    users :: account:db(),
    active :: dict:dict(pid(), account:id()),
    channel :: comms:channel()
}).

%%--------------------------------------------------------------------------

-endif.
