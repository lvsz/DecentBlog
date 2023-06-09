-module(server_tests).

-compile([export_all, nowarn_export_all]).

-include("account.hrl").

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

new_test() ->
    ID = ?FUNCTION_NAME,
    ?assertMatch(
        #{id := ID, users := _, active := #{}, channel := _},
        server:new(ID)
    ).

test_registration(Account, ServerID) ->
    ?assert(lists:member(ServerID, registered()), "is server reachable"),
    ServerID ! {self(), register, Account},
    ?LOG_INFO("Testing user registrating", [Account]),
    Response =
        receive
            {_Sender, registered, NewAccount} ->
                ?LOG_DEBUG("Account registered", [NewAccount]),
                {ok, NewAccount};
            BadResponse ->
                ?LOG_WARNING(["Received unexpected message", BadResponse]),
                {fail, {unexptected, BadResponse}}
        after 1000 ->
            ?LOG_WARNING("Response time out"),
            {fail, no_response}
        end,
    UID = Account#account.id,
    ?assertMatch({ok, #account{id = UID, server = ServerID}}, Response),
    Response.
