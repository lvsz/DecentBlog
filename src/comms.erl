-module(comms).
-export([new_channel/1, new_follow_list/0, follow/2, get_posts/2]).
-export_type([scope/0, channel/0, follow_list/0]).
-include("comms.hrl").

-type scope() :: public | server | account | invite.
-type channel() :: #channel{}.
-type follow_list() :: [reference()].

-spec new_follow_list() -> follow_list().
new_follow_list() -> [].

-spec new_channel(Scope :: scope()) -> channel().
new_channel(Scope) -> #channel{scope = Scope}.

-spec new(Host :: server:id(), Scope :: scope()) -> channel().
new(Host, Scope) -> #channel{host = Host, scope = Scope}.

-spec get_posts(Scope :: scope(), Channel :: channel()) -> [#blogpost{}].
get_posts(all, #channel{posts = Posts}) ->
    Posts;
get_posts(Scope, #channel{posts = Posts}) ->
    lists:filter(fun(#blogpost{scope = S}) -> Scope == S end, Posts).

-spec follow(Channel :: channel(), Subs :: follow_list()) -> follow_list().
follow(Channel, Subs) ->
    case lists:member(Channel, Subs) of
        true -> Subs;
        false -> [Channel | Subs]
    end.
