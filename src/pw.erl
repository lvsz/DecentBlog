-module(pw).
-export([sign/2, verify/2]).
-export_type([pw/0]).
-include("account.hrl").

%-spec init(ServerName) -> no_return().
%init(ServerName) -> .

-type pw() :: binary().

encode(Text) -> crypto:hash(blake2s, Text).

-spec sign(Account :: #account{}, PlainText :: string()) -> #account{}.
sign(Account, PlainText) -> Account#account{password = encode(PlainText)}.

-spec verify(Account :: #account{}, Password :: pw()) -> boolean().
verify(#account{password = Password}, Password) -> true;
verify(_Account, _Password) -> false.
