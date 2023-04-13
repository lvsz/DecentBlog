-module(pw).
-export([sign/2, verify/2]).
-export_type([pw/0]).
-include("account.hrl").

%-spec init(ServerName) -> no_return().
%init(ServerName) -> .

-type pw() :: binary().

encode(Text) -> crypto:hash(blake2s, Text).

-spec sign(PlainText, Account1) -> Account2 when
    PlainText :: string(),
    Account1 :: account:account(),
    Account2 :: account:account().
sign(Account, PlainText) ->
    Account#{password => encode(PlainText)}.

-spec verify(Account, Password) -> boolean() when
    Account :: account:account(),
    Password :: pw:pw().
verify(Account, Password) ->
    maps:get(password, Account) == Password.
