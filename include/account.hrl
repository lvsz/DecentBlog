%%%----FILE account.hrl----

%%%

-ifndef(ACCOUNT_HRL_).
-define(ACCOUNT_HRL_, 1).
%%--------------------------------------------------------------------------

-record(account, {
    id :: account:id(),
    server :: server:id(),
    password :: binary(),
    subscriptions :: comms:follow_list()
}).

%%--------------------------------------------------------------------------
-endif.
