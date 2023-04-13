-module(server).

-export([new/1, run/1]).
-export_type([id/0, data/0]).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("server.hrl").
-include("account.hrl").

-type id() :: atom().
-type data() :: #{
    id := id(),
    users := account:db(),
    active := #{pid() => account:id()},
    channel := comms:channel()
}.

-spec new(ID :: server:id()) -> server:data().
new(ID) ->
    #{
        id => ID,
        users => account:new_db(),
        active => #{},
        channel => comms:new_channel(server)
    }.

-spec run(Data :: server:data()) -> pid().
run(Data) -> spawn_link(fun() -> actor(Data) end).

-spec actor(Data :: server:data()) -> no_return().
actor(Data) ->
    receive
        %% create new account
        {Sender, account_register, Account} ->
            NewData = account:register(Sender, Account, Data),
            actor(NewData);
        %% log in with existing account
        {Sender, account_login, Username, Password} ->
            NewData = account:login(Sender, Username, Password, Data),
            actor(NewData);
        %% log out
        {Sender, account_logout, Username} ->
            NewData = account:logout(Sender, Username, Data),
            actor(NewData);
        {Sender, post_put, Post} ->
            NewData = comms:post_put(Sender, Post, Data),
            actor(NewData);
        {Sender, post_get} ->
            NewData = comms:post_get(Sender, all, Data),
            actor(NewData);
        % retrieve posts based on user privilege
        {Sender, kill} ->
            Sender ! {self(), ok}
    end.
