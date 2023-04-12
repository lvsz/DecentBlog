-module(server).

-export([new/1, run/1]).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("server.hrl").
-include("account.hrl").

-type id() :: atom().
-type data() :: #{
    id => id(),
    users => account:db(),
    active => #{pid() => account:id()},
    channel => comms:channel()
}.

-spec new(ID :: id()) -> data().
new(ID) ->
    #{
        id => ID,
        users => account:new_db(),
        active => #{},
        channel => comms:new_channel(server)
    }.

-spec run(Data :: data()) -> pid().
run(Data) -> spawn_link(fun() -> actor(Data) end).

-spec actor(Data :: data()) -> no_return().
actor(Data) ->
    receive
        %% create new account
        {Sender, create, Account} ->
            NewData = create_account(Sender, Account, Data),
            actor(NewData);
        %% log in with existing account
        {Sender, login, Username, Password} ->
            NewData = login_user(Sender, Username, Password, Data),
            actor(NewData);
        %% log out
        {Sender, logout, Username} ->
            NewData = logout_user(Sender, Username, Data),
            actor(NewData);
        {Sender, post, Post} ->
            NewData = server_post(Sender, Post, Data),
            actor(NewData);
        {Sender, get_posts} ->
            actor(Data);
        %% retrieve posts based on user privilege
        {Sender, kill} ->
            Sender ! {self(), ok}
    end.

-spec create_account(Sender, Account, Data) -> Data when
    Sender :: pid(),
    Account :: account:account(),
    Data :: data().
create_account(Sender, Account, Data) ->
    SID = maps:get(id, Data),
    UserDB = maps:get(users, Data),
    NewAccount = account:add_server(SID, Account),
    case account:db_store(NewAccount, UserDB) of
        {ok, NewUserDB} ->
            % Add account to user database and active user set
            Sender ! {self(), create_account, {ok, NewAccount}},
            Active = maps:get(active, Data),
            Data#{
                users => NewUserDB,
                active => Active#{Sender => Account#account.id}
            };
        error ->
            Sender ! {self(), create_account, {fail, username_taken}},
            Data
    end.
create_account_test() ->
    SID = ?FUNCTION_NAME,
    UID = test_user,
    Acc1 = ?debugVal(account:new(UID, <<"foo">>)),
    Data1 = ?debugVal(create_account(self(), Acc1, new(SID))),
    receive
        Msg1 -> ?debugVal(Msg1)
    end,
    ?assertMatch({_, create_account, {ok, _}}, Msg1),
    {_, _, {_, NewAcc1}} = Msg1,
    ?assertNotEqual(Acc1, NewAcc1, "Account should be updated"),
    ?assertEqual(UID, NewAcc1#account.id, "User ID should stay the same"),
    ?assertEqual(SID, NewAcc1#account.server, "Account should include server"),
    %% TODO: move UserDB from account to server
    %%?assertEqual(Acc1, account:db_fetch(UID, Data1#server.users)),
    Acc2 = account:new(UID, <<"bar">>),
    Data2 = create_account(self(), Acc2, Data1),
    receive
        Msg2 -> Msg2
    end,
    ?assertMatch({_, create_account, {fail, username_taken}}, Msg2),
    ?assertEqual(Data1, Data2, "Data should be unchanged after failure").

-spec login_user(Sender, Username, Password, data()) -> data() when
    Sender :: pid(),
    Username :: account:id(),
    Password :: pw:pw().
login_user(Sender, Username, Password, Data) ->
    #server{users = UserDB, active = Active} = Data,
    NotActive = not maps:is_key(Sender, Active),
    case catch pw:verify(account:db_fetch(Username, UserDB), Password) of
        {ok, Account} when NotActive ->
            Sender ! {self(), login, {ok, Account}},
            Data#server{active = maps:put(Sender, Username, Active)};
        Else ->
            case Else of
                {ok, Account} ->
                    ?LOG_WARNING("Already logged in: ~w => ~w", [Username, Account]),
                    Sender ! {self(), login, {fail, logged_in}};
                {error, bad_pw} ->
                    Sender ! {self(), login, {fail, bad_pw}};
                {'EXIT', _Reason} ->
                    Sender ! {self(), login, {fail, bad_id}}
            end,
            Data
    end.

-spec logout_user(Sender, Username, data()) -> data() when
    Sender :: pid(),
    Username :: account:id().
logout_user(Sender, Username, Data) ->
    case maps:take(Sender, Data#server.active) of
        {UID, NewActive} when UID == Username ->
            Sender ! {self(), logout, ok},
            Data#server{active = NewActive};
        {UID, _} ->
            ?LOG_WARNING("Logout username mismatch: ~w => ~w", [Username, UID]),
            Sender ! {self(), logout, {fail, bad_id}},
            Data;
        error ->
            ?LOG_WARNING("User already logged out: ~w", [Username]),
            Sender ! {self(), logout, {fail, logged_out}},
            Data
    end.

-spec server_post(Sender, Post, data()) -> data() when
    Sender :: pid(),
    Post :: comms:post().
server_post(Sender, Post, Data) ->
    case maps:is_key(Sender, maps:get(active, Data)) of
        true ->
            NewChannel = comms:post_to(Post, maps:get(channel, Data)),
            Sender ! {self(), post, ok},
            Data#{channel => NewChannel};
        false ->
            ?LOG_WARNING("User not logged in: ~w", [maps:get(user, Post)]),
            Sender ! {self(), post, {fail, logged_out}},
            Data
    end.
server_post_test() ->
    Data0 = new(test_data),
    Post1 = comms:new_post(user1, <<"Lorem ipsum">>),
    Data1 = ?debugVal(server_post(self(), Post1, Data0#{active => #{self() => user1}})),
    ?debugVal(maps:get(channel, Data1)),
    ?assertEqual([Post1], comms:get_posts(1, maps:get(channel, Data1))),
    Post2 = comms:new_post(user2, "HELLO WORLD"),
    Data2 = server_post(self(), Post2, Data1),
    ?assertEqual([Post2, Post1], comms:get_posts(2, maps:get(channel, Data2))).

%        {Sender, get_posts} ->
%            case dict:is_key(Sender, Data#server.active) of
%                true -> Scope = all;
%                false -> Scope = public
%            end,
%            Posts = comms:get_posts(Scope, Data#server.channel),
%            Sender ! {self(), posts, Posts},
%            actor(Data);

server_test_() ->
    %Pid = run(new_record(test_server)),
    [].
