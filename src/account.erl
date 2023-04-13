-module(account).

-export([new/2, add_server/2, new_db/0, db_store/2, db_fetch/2]).
-export([register/3, login/4, logout/3]).
-export_type([id/0, db/0]).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("account.hrl").
-include("server.hrl").
%-record(account, {
%    id :: account:id(),
%    server :: server:id() | undefined,
%    password :: binary(),
%    subscriptions :: comms:follow_list()
%}).

-type id() :: atom().
-type db() :: dict:dict(id(), #account{}).
-type account() :: #account{}.

-define(getID(Account), Account#account.id).

-spec new(Name :: id(), Password :: binary()) -> account().
new(Name, Password) ->
    #account{
        id = Name,
        password = Password,
        server = undefined
    }.

-spec new_db() -> db().
new_db() -> dict:new().

-spec add_server(ServerID, account()) -> account() when
    ServerID :: server:id().
add_server(ServerID, Account) ->
    case Account of
        #account{server = undefined} ->
            Account#account{server = ServerID};
        #account{} ->
            ?LOG_ERROR("Account already assigned a server: ~w", [Account]),
            error(server_defined, [Account, ServerID])
    end.

-spec db_store(Account, db()) -> {ok, db()} | error when
    Account :: account().
db_store(Account, DB) ->
    UID = ?getID(Account),
    case dict:is_key(UID, DB) of
        true -> error;
        false -> {ok, dict:store(UID, Account, DB)}
    end.

-spec db_fetch(UID :: id(), DB :: db()) -> account().
db_fetch(UID, DB) -> dict:fetch(UID, DB).

-spec register(Sender, Account, Data1) -> Data2 when
    Sender :: pid(),
    Account :: account(),
    Data1 :: server:data(),
    Data2 :: server:data().
register(Sender, Account, Data) ->
    SID = maps:get(id, Data),
    UserDB = maps:get(users, Data),
    NewAccount = add_server(SID, Account),
    case db_store(NewAccount, UserDB) of
        {ok, NewUserDB} ->
            % Add account to user database and active user set
            Sender ! {self(), account_register, {ok, NewAccount}},
            Active = maps:get(active, Data),
            Data#{
                users => NewUserDB,
                active => Active#{Sender => Account#account.id}
            };
        error ->
            Sender ! {self(), account_register, {fail, username_taken}},
            Data
    end.

-spec login(Sender, Username, Password, server:data()) -> server:data() when
    Sender :: pid(),
    Username :: account:id(),
    Password :: pw:pw().
login(Sender, Username, Password, Data) ->
    #server{users = UserDB, active = Active} = Data,
    NotActive = not maps:is_key(Sender, Active),
    case catch pw:verify(db_fetch(Username, UserDB), Password) of
        {ok, Account} when NotActive ->
            Sender ! {self(), account_login, {ok, Account}},
            Data#server{active = maps:put(Sender, Username, Active)};
        Else ->
            case Else of
                {ok, Account} ->
                    ?LOG_WARNING("Already logged in: ~w => ~w", [Username, Account]),
                    Sender ! {self(), account_login, {fail, logged_in}};
                {error, bad_pw} ->
                    Sender ! {self(), account_login, {fail, bad_pw}};
                {'EXIT', _Reason} ->
                    Sender ! {self(), account_login, {fail, bad_id}}
            end,
            Data
    end.

-spec logout(Sender, Username, server:data()) -> server:data() when
    Sender :: pid(),
    Username :: account:id().
logout(Sender, Username, Data) ->
    case maps:take(Sender, Data#server.active) of
        {UID, NewActive} when UID == Username ->
            Sender ! {self(), account_logout, ok},
            Data#server{active = NewActive};
        {UID, _} ->
            ?LOG_WARNING("account_logout username mismatch: ~w => ~w", [Username, UID]),
            Sender ! {self(), account_logout, {fail, bad_id}},
            Data;
        error ->
            ?LOG_WARNING("User already logged out: ~w", [Username]),
            Sender ! {self(), account_logout, {fail, logged_out}},
            Data
    end.
