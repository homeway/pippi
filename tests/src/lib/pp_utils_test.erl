%% -*- mode: nitrogen -*-
-module(pp_utils_test).
-include_lib("eunit/include/eunit.hrl").

allow_test() ->
    Methods = [
        users1,
        [friends1, all],
        [contacts1, [all, get]],
        [orders1, [[get, <<"001">>], [send, <<"001">>, <<"abc">>], all]],
        [[users2, <<"001">>], get]
    ],

    ?assertNot(pp:allow([myuser, all], Methods)),

    ?assert(pp:allow([users1, all], Methods)),
    ?assert(pp:allow([users1, get, <<"001">>], Methods)),
    ?assert(pp:allow([users1, create], Methods)),

    ?assert(pp:allow([friends1, all], Methods)),
    ?assertNot(pp:allow([friends1, get], Methods)),

    ?assert(pp:allow([contacts1, all], Methods)),
    ?assertNot(pp:allow([contacts1, create], Methods)),

    ?assertNot(pp:allow([orders1, get], Methods)),
    ?assertNot(pp:allow([orders1, get, [<<"002">>]], Methods)),
    ?assert(pp:allow([orders1, get, [<<"001">>]], Methods)),
    ?assertNot(pp:allow([orders1, send, [<<"001">>, <<"002">>]], Methods)),
    ?assert(pp:allow([orders1, send, [<<"001">>, <<"abc">>]], Methods)),
    ?assert(pp:allow([[nosqlite, users1], all], Methods)),
    ?assertNot(pp:allow([[nosqlite, friends1], find], Methods)),
    ?assert(pp:allow([[nosqlite, orders1], get, [<<"001">>]], Methods)).


