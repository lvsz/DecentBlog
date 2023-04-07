-module(account).
-include("account.hrl").
-export([new/3, new_db/0, db_store/2]).
-export_type([id/0, db/0]).

-type id() :: atom().
-type db() :: dict:dict(id(), #account{}).

-spec new(Name :: id(), Server :: server:id(), Password :: binary()) -> #account{}.
new(Name, Server, Password) ->
    #account{id = Name, server = Server, password = Password}.

-spec new_db() -> db().
new_db() -> dict:new().

-spec db_store(Account :: #account{}, UserDB :: db()) -> db().
db_store(Account, UserDB) ->
    Username = Account#account.id,
    case dict:is_key(Username, UserDB) of
        true -> {fail, username_taken};
        false -> dict:store(Username, Account, UserDB)
    end.
