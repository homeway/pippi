%% -*- mode: nitrogen -*-
-module(pp_utils_test).
-include_lib("eunit/include/eunit.hrl").

to_string_test() ->
    ?assertEqual("123", pp:to_string(123)),
    ?assertEqual("12.3", pp:to_string(12.3)),
    ?assertEqual("abc", pp:to_string(abc)),
    ?assertEqual("\"abc\"", pp:to_string("abc")),
    ?assertEqual("<<\"abc\">>", pp:to_string(<<"abc">>)),

    ?assertEqual("[]", pp:to_string([])),
    ?assertEqual("[1, 2]", pp:to_string([1,2])),
    ?assertEqual("\"ab\"", pp:to_string([$a,$b])),
    ?assertEqual("[\"1\", \"2\"]", pp:to_string(["1", "2"])),

    ?assertEqual("{1, 2}", pp:to_string({1,2})),

    ?assertEqual("#{a => 1, b => 2}", pp:to_string(#{a=>1, b=>2})),
    ?assertEqual("#{name => \"123\"}", pp:to_string(#{name=>"123"})),
    ?assertEqual("#{\"name\" => \"123\"}", pp:to_string(#{"name"=>"123"})),

    ?assertEqual("[[a, b], c]", pp:to_string([[a, b], c])),
    ?assertEqual("[ab, {bc, cd}]", pp:to_string([ab, {bc, cd}])),
    ?assertEqual("[ab, #{a => 1, b => 2}]", pp:to_string([ab, #{a=>1, b=>2}])),

    ?assertEqual("{{a, b}, c}", pp:to_string({{a, b}, c})),
    ?assertEqual("{ab, {bc, cd}}", pp:to_string({ab, {bc, cd}})),
    ?assertEqual("{ab, #{a => 1, b => 2}}", pp:to_string({ab, #{a=>1, b=>2}})).


apply_test() ->
    ?assertMatch([], pp:apply([nosqlite, users], all, [])).

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


