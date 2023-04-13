-module(server).

-export([new/1, run/1]).
-export_type([id/0, data/0]).

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
            NewData = post_put(Sender, Post, Data),
            actor(NewData);
        {Sender, post_get} ->
            actor(Data);
        % retrieve posts based on user privilege
        {Sender, kill} ->
            Sender ! {self(), ok}
    end.

-spec post_put(Sender, Post, data()) -> data() when
    Sender :: pid(),
    Post :: comms:post().
post_put(Sender, Post, Data) ->
    case maps:is_key(Sender, maps:get(active, Data)) of
        true ->
            NewChannel = comms:post_put(Post, maps:get(channel, Data)),
            Sender ! {self(), post, ok},
            Data#{channel => NewChannel};
        false ->
            ?LOG_WARNING("User not logged in: ~w", [maps:get(user, Post)]),
            Sender ! {self(), post, {fail, logged_out}},
            Data
    end.
post_put_test() ->
    Data0 = new(test_data),
    Post1 = comms:new_post(user1, <<"Lorem ipsum">>),
    Data1 = ?debugVal(post_put(self(), Post1, Data0#{active => #{self() => user1}})),
    ?debugVal(maps:get(channel, Data1)),
    ?assertEqual([Post1], comms:post_get(1, maps:get(channel, Data1))),
    Post2 = comms:new_post(user2, "HELLO WORLD"),
    Data2 = post_put(self(), Post2, Data1),
    ?assertEqual([Post2, Post1], comms:post_get(2, maps:get(channel, Data2))).

%        {Sender, post_get} ->
%            case dict:is_key(Sender, Data#server.active) of
%                true -> Scope = all;
%                false -> Scope = public
%            end,
%            Posts = comms:post_get(Scope, Data#server.channel),
%            Sender ! {self(), posts, Posts},
%            actor(Data);

server_test_() ->
    %Pid = run(new_record(test_server)),
    [].
