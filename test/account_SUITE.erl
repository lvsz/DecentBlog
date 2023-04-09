-module(account_SUITE).

-compile([export_all, nowarn_export_all]).

%-include("account.hrl").
%-include("server.hrl").

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

initialize_test() ->
    logger:set_primary_config(level, debug),
    ?LOG_DEBUG("Start initialize_test()"),
    ?assert(1 == 1),
    io:fwrite("done~n").

account_test() ->
    ?LOG_DEBUG("account_test()"),
    initialize_test(),
    ?assert(2 == 2),
    io:fwrite("done~n").

