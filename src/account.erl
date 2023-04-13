-module(account).

-export([new/2, add_server/2, new_db/0, db_store/2, db_fetch/2]).
-export([register/3, login/4, logout/3]).
-export_type([account/0, id/0, db/0]).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-type id() :: atom().
-type account() :: #{
    id := account:id(),
    password := pw:pw(),
    server := server:id()
}.
-type db() :: dict:dict(id(), account()).

-define(getID(Account), map_get(id, Account)).

-spec new(Name :: id(), Password :: binary()) -> account().
new(Name, Password) ->
    #{
        id => Name,
        password => Password,
        server => undefined
    }.

-spec new_db() -> db().
new_db() -> dict:new().

-spec add_server(ServerID, account()) -> account() when
    ServerID :: server:id().
add_server(ServerID, Account) ->
    case Account of
        #{server := undefined} ->
            Account#{server := ServerID};
        #{} ->
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

-spec db_fetch(UID :: account:id(), DB :: account:db()) -> account().
db_fetch(UID, DB) -> dict:fetch(UID, DB).

-spec register(Sender, Account, Data1) -> Data2 when
    Sender :: pid(),
    Account :: account(),
    Data1 :: server:data(),
    Data2 :: server:data().
register(Sender, Account, Data) ->
    SID = map_get(id, Data),
    UserDB = map_get(users, Data),
    NewAccount = add_server(SID, Account),
    case db_store(NewAccount, UserDB) of
        {ok, NewUserDB} ->
            % Add account to user database and active user set
            Sender ! {self(), account_register, {ok, NewAccount}},
            Active = map_get(active, Data),
            Data#{
                users := NewUserDB,
                active := Active#{Sender => map_get(id, Account)}
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
    #{users := UserDB, active := Active} = Data,
    case catch pw:verify(db_fetch(Username, UserDB), Password) of
        {ok, Account} when not is_map_key(Sender, Active) ->
            Sender ! {self(), account_login, {ok, Account}},
            Data#{active := Active#{Sender => Username}};
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

-spec logout(Sender, Username, Data1) -> Data2 when
    Sender :: pid(),
    Username :: account:id(),
    Data1 :: server:data(),
    Data2 :: server:data().
logout(Sender, Username, Data) ->
    Active = map_get(active, Data),
    case maps:take(Sender, Active) of
        {UID, NewActive} when UID == Username ->
            Sender ! {self(), account_logout, ok},
            Data#{active := NewActive};
        {UID, _} ->
            ?LOG_WARNING("account_logout username mismatch: ~w => ~w", [Username, UID]),
            Sender ! {self(), account_logout, {fail, bad_id}},
            Data;
        error ->
            ?LOG_WARNING("User already logged out: ~w", [Username]),
            Sender ! {self(), account_logout, {fail, logged_out}},
            Data
    end.
