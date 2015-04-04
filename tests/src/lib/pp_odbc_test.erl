-module(pp_odbc_test).
-include_lib("eunit/include/eunit.hrl").

main_test() ->
    odbc:start(),
    {ok, Ref} = odbc:connect("DSN=myebig;UID=tayy;PWD=tayy", []),
    odbc:disconnect(Ref).
