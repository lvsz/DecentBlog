-module(account).
-include("account.hrl").
-export([new/2, new_db/0, db_store/2]).
-export_type([id/0, db/0]).

-type id() :: atom().
-type db() :: dict:dict(id(), #account{}).

-spec new(Name :: id(), Password :: binary()) -> #account{}.
new(Name, Password) -> #account{id = Name, password = Password}.

-spec new_db() -> db().
new_db() -> dict:new().

-spec db_store(User :: #account{}, DB :: db()) -> {ok, db()} | {fail, atom()}.
db_store(User, DB) ->
    UID = User#account.id,
    case dict:is_key(UID, DB) of
        true -> {fail, username_taken};
        false -> {ok, dict:store(UID, User, DB)}
    end.

