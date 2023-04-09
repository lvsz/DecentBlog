%%%----FILE main.erl----

%%%

-module(main_SUITE).

-compile([export_all, nowarn_export_all]).

-include("account.hrl").

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

initialize_test() ->
    logger:set_primary_config(level, debug),
    ?LOG_DEBUG("Start initialize_test()"),
    Server1 = server:new(test_server_1),
    Server2 = server:new(test_server_2),
    Server3 = server:new(test_server_3),
    main:initialize([Server1, Server2, Server3]),
    ?LOG_DEBUG("Servers initialized"),
    Account1 = #account{id = test_account_1, password = <<"test_1">>},
    Account2 = #account{id = test_account_2, password = <<"test_2">>},
    ?assert(Account1#account.id == test_account_1),
    test_server_1 ! {self(), register, Account1},
    test_server_1 ! {self(), register, Account2}.
