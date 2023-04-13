-module(account_tests).

-include("account.hrl").
-include_lib("eunit/include/eunit.hrl").

register_test() ->
    SID = ?FUNCTION_NAME,
    UID = test_user,
    Acc1 = account:new(UID, <<"foo">>),
    Data1 = account:register(self(), Acc1, server:new(SID)),
    receive
        Msg1 -> Msg1
    end,
    ?assertMatch({_, account_register, {ok, _}}, Msg1),
    {_, _, {_, NewAcc1}} = Msg1,
    ?assertNotEqual(Acc1, NewAcc1, "Account should be updated"),
    ?assertEqual(UID, NewAcc1#account.id, "User ID should stay the same"),
    ?assertEqual(SID, NewAcc1#account.server, "Account should include server"),
    %% TODO: move UserDB from account to server
    %%?assertEqual(Acc1, account:db_fetch(UID, Data1#server.users)),
    Acc2 = account:new(UID, <<"bar">>),
    Data2 = account:register(self(), Acc2, Data1),
    receive
        Msg2 -> Msg2
    end,
    ?assertMatch({_, account_register, {fail, username_taken}}, Msg2),
    ?assertEqual(Data1, Data2, "Data should be unchanged after failure").
