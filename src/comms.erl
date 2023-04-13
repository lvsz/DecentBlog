-module(comms).
-export([new_post/2, new_channel/1, post_put/3, post_get/3]).
-export_type([scope/0, channel/0]).

-include_lib("eunit/include/eunit.hrl").

-type scope() :: public | server | account | invite.
-type channel(Scope) :: {Scope, [post()]}.
-type channel() :: [post()].
-type post() :: #{
    user => user:id(),
    text => string()
}.

-spec new_post(UserID, Text) -> post() when
    UserID :: user:id(),
    Text :: string().
new_post(UserID, Text) ->
    #{user => UserID, text => Text}.

-spec new_channel(Scope :: scope()) -> channel().
new_channel(Scope) -> {Scope, []}.

-spec post_put(Sender, Post, Data1) -> Data2 when
    Sender :: pid(),
    Post :: post(),
    Data1 :: server:data(),
    Data2 :: server:data().
post_put(Sender, Post, Data) ->
    %% TODO: verify account id and login before posting
    Channel = maps:get(channel, Data),
    Sender ! {self(), post_put, ok},
    Data#{channel => [Post | Channel]}.

-spec post_get(Sender, Amount, Data) -> Data when
    Sender :: pid(),
    Amount :: non_neg_integer() | all,
    Data :: server:data().
post_get(Sender, Amount, Data) ->
    Posts = maps:get(channel, Data),
    Sender ! lists:sublist(Posts, Amount),
    Data.

%-spec new_follow_list() -> follow_list().
%new_follow_list() -> [].
%-spec follow(Channel :: channel(), Subs :: follow_list()) -> follow_list().
%follow(Channel, Subs) ->
%    case lists:member(Channel, Subs) of
%        true -> Subs;
%        false -> [Channel | Subs]
%    end.
%
