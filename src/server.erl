-module(server).

-export([actor/1, new/1]).

-include_lib("kernel/include/logger.hrl").

-include("comms.hrl").
-include("server.hrl").
-include("account.hrl").

-type id() :: atom().

-spec post_blogpost(Blogpost :: #blogpost{}, ServerPID :: pid()) -> ok.
post_blogpost(Blogpost, ServerPID) ->
    todo.

-spec new(ID :: id()) -> #server{}.
new(ID) ->
    #server{
        id = ID,
        users = account:new_db(),
        active = dict:new(),
        channel = comms:new_channel(server)
    }.

-spec actor(Data :: #server{}) -> no_return().
actor(Data) ->
    receive
        %% register new account
        {Sender, register, Account} ->
            case register_user(Sender, Account, Data) of
                {ok, NewAccount, NewData} ->
                    ?LOG_DEBUG("ok: Register account", [Account#account.id]),
                    Sender ! {self(), registered, NewAccount},
                    actor(NewData);
                {fail, username_taken} ->
                    ?LOG_DEBUG("fail: Register account", [Account#account.id]),
                    Sender ! {self(), username_taken},
                    actor(Data)
            end;
        %% log in with existing account
        {Sender, login, Username, Password} ->
            case login_user(Data, Username, Password) of
                {ok, Account} ->
                    Active = dict:store(Username, Sender, Data#server.active),
                    Sender ! {self(), login_ok, Account},
                    actor(Data#server{active = Active});
                {fail, Reason} ->
                    Sender ! {self(), login_fail, Reason},
                    actor(Data)
            end;
        %% log out
        {Sender, logout, Username} ->
            Sender ! {self(), logged_out},
            NewData = Data#server{
                active = sets:del_element(Username, Data#server.active)
            },
            actor(NewData);
        %{Sender, post, Channel, Post} ->
        %    case Channel of
        %        internal ->
        %            NewData = Data#server{
        %                internal_channel = [Post | Data#server.internal_channel]
        %            },
        %            Sender ! {self(), posted};
        %        external ->
        %            NewData = Data#server{
        %                external_channel = [Post | Data#server.external_channel]
        %            },
        %            Sender ! {self(), posted}
        %    end,
        %    actor(NewData);
        %% retrieve posts based on user privilege
        {Sender, get_posts} ->
            case dict:is_key(Sender, Data#server.active) of
                true -> Scope = all;
                false -> Scope = public
            end,
            Posts = comms:get_posts(Scope, Data#server.channel),
            Sender ! {self(), posts, Posts},
            actor(Data)
    end.

register_user(Sender, Account, Data) ->
    UserDB = Data#server.users,
    NewAccount = Account#account{server = Data#server.id},
    case account:db_store(NewAccount, UserDB) of
        {ok, NewUserDB} ->
            NewData = Data#server{
                users = NewUserDB,
                active = dict:store(Sender, Account#account.id, Data#server.active)
            },
            {ok, NewAccount, NewData};
        {fail, username_taken} ->
            {fail, username_taken};
        ERROR ->
            ?LOG_ALERT("Uknown value", ERROR),
            {fail, ERROR}
    end.

login_user(Data, Username, Password) ->
    UserDB = Data#server.users,
    case catch pw:verify(dict:fetch(Username, UserDB), Password) of
        {fail, bad_pw} -> {fail, bad_pw};
        {'EXIT', _Reason} -> {fail, bad_id};
        {ok, Account} -> {ok, Account}
    end.
