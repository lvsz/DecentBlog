-module(account).

-export([new/2, add_server/2, new_db/0, db_store/2, db_fetch/2]).
-export_type([id/0, db/0]).

-include_lib("kernel/include/logger.hrl").
-include("account.hrl").
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
