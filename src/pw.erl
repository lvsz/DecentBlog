-module(pw).
-export([sign/2, verify/2]).
-include("account.hrl").

%-spec init(ServerName) -> no_return().
%init(ServerName) -> .

encode(Text) -> crypto:hash(blake2s, Text).

-spec sign(Account :: #account{}, PlainText :: string()) -> #account{}.
sign(Account, PlainText) -> Account#account{password = encode(PlainText)}.

-spec verify(Account :: #account{}, Password :: binary()) -> boolean().
verify(#account{password = Password}, Password) -> true;
verify(_Account, _Password) -> false.
