%%%----FILE main.erl----

%%%

-module(main_SUITE).

-compile([export_all, nowarn_export_all]).

-include("account.hrl").
-include("server.hrl").

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec registered(ID :: atom()) -> boolean().
registered(ID) -> lists:member(ID, registered()).

-spec test_init(server:id()) -> ok.
test_init(ServerIDs) ->
    ?LOG_DEBUG("~w CALLED", [?FUNCTION_NAME]),
    ?assertNot(registered(?MAIN), "Main actor shouldn't be running yet"),
    main:run(),
    ?assert(registered(?MAIN), "Assert main actor is active"),
    lists:foreach(
        fun(X) -> ?MAIN ! {self(), initialize, server:new(X)} end,
        ServerIDs
    ),
    Sync = fun(X) ->
        receive
            {_, initialized, X} -> ok
        end
    end,
    lists:foreach(Sync, ServerIDs).

test_registration_helper(ServID) ->
    ?LOG_DEBUG("~w CALLED", [?FUNCTION_NAME]),
    fun(User) ->
        UserID = User#account.id,
        RegistrationTest = server_tests:test_registration(User, ServID),
        ?assertMatch(
            {ok, #account{id = UserID, server = ServID}},
            RegistrationTest,
            "verify user registration"
        ),
        case RegistrationTest of
            {ok, NewUser} ->
                {true, NewUser};
            Response ->
                ?LOG_WARNING("Registration of ~w failed: ~w", [User, Response]),
                false
        end
    end.

-define(_BOB, 'BOB').
test_registration(Servers) ->
    ?LOG_DEBUG("~w CALLED", [?FUNCTION_NAME]),
    ?assertMatch([_, _ | _], Servers, "We need at least 2 servers"),
    [Serv1, Serv2 | _] = Servers,
    UIDs = [?_BOB, 'Alice', foo],
    PWs = [<<"hijHsd98*WAd-y+2ohRiw">>, <<"<π>.][#]£({">>, <<"hunter2">>],
    Accounts = lists:zipwith(fun(ID, PW) -> account:new(ID, PW) end, UIDs, PWs),
    FirstResults = lists:filtermap(test_registration_helper(Serv1), Accounts),
    OtherBOB = account:new(?_BOB, term_to_binary(?_BOB)),
    %% test adding duplicate names
    ?assertNotMatch(
        {ok, _Account},
        server_tests:test_registration(OtherBOB, Serv1),
        "No duplicate usernames allowed on one server"
    ),
    case (test_registration_helper(Serv2))(OtherBOB) of
        {true, OkBob} ->
            Results = [{ok, OkBob} | FirstResults];
        false ->
            ?LOG_WARNING("Failled to add user to second server"),
            Results = FirstResults
    end,
    Results.

main_SUITE_test() ->
    logger:set_primary_config(level, all),
    ?LOG_NOTICE("STARTING ~w TESTS", [?MODULE]),
    ServerList = test_init(),
    _Registrations = test_registration(ServerList),
    ?LOG_NOTICE("COMPLETED ~w TESTS", [?MODULE]).
