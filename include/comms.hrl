%%%----FILE comms.hrl----

%%%

-ifndef(COMMS_HRL_).
-define(COMMS_HRL_, 1).
%%--------------------------------------------------------------------------

-type post_ref() :: reference().

-record(metrics, {
    liked_by = [] :: [user:id()],
    comments = [] :: [post_ref()]
}).

-record(blogpost, {
    ref = make_ref() :: reference(),
    user :: user:id(),
    text = "" :: string(),
    scope :: comms:scope(),
    tags = [] :: [term()],
    mentions = [] :: [user:id()],
    metrics = #metrics{} :: #metrics{}
}).

-record(channel, {
    ref = make_ref() :: reference(),
    host :: server:id(),
    scope :: comms:scope(),
    admins = [] :: [user:id()],
    members = [] :: [user:id()],
    posts = [] :: [#blogpost{}]
}).

%%--------------------------------------------------------------------------
-endif.
