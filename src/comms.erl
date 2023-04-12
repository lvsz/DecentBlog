-module(comms).
-export([new_channel/1, new_follow_list/0, follow/2, get_posts/2, post_to/2]).
-export_type([scope/0, channel/0, follow_list/0]).
-include("comms.hrl").

-type channel(Scope) :: {Scope, [post()]}.
-type channel() :: channel(_).
-type post() :: #{
    user => user:id(),
    text => string()
}.
-type scope() :: public | server | account | invite.
-type follow_list() :: [reference()].

-spec new_channel(Scope :: scope()) -> channel().
new_channel(Scope) -> {Scope, []}.

-spec post_to(Post, Channel1) -> Channel2 when
    Post :: post(),
    Channel1 :: channel(Scope),
    Channel2 :: channel(Scope).
post_to(Post, Channel) ->
    {Scope, Posts} = Channel,
    {Scope, [Post | Posts]}.

-spec get_posts(Number, Channel) -> Posts when
    Number :: non_neg_integer(),
    Channel :: channel(),
    Posts :: [post()].
get_posts(Number, Channel) ->
    {_Scope, Posts} = Channel,
    lists:sublist(Posts, Number).

-spec new_follow_list() -> follow_list().
new_follow_list() -> [].
-spec follow(Channel :: channel(), Subs :: follow_list()) -> follow_list().
follow(Channel, Subs) ->
    case lists:member(Channel, Subs) of
        true -> Subs;
        false -> [Channel | Subs]
    end.
