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

allow_test() ->
    Methods = [
        users1,
        [friends1, all],
        [contacts1, [all, get]],
        [orders1, [[get, <<"001">>], [send, <<"001">>, <<"abc">>], all]],
        [[users2, <<"001">>], get]
    ],

    %% not exist
    ?assertNot(pp:allow([myuser, all], Methods)),

    %% [users1]
    ?assert(pp:allow([users1, all], Methods)),
    ?assert(pp:allow([users1, get, [<<"001">>]], Methods)),
    ?assert(pp:allow([users1, create, [#{}]], Methods)),

    %% [friends, all]
    ?assert(pp:allow([friends1, all], Methods)),
    %% not enough params
    ?assertNot(pp:allow([friends1, get], Methods)),

    %% [contacts, [all, create]]
    ?assert(pp:allow([contacts1, all], Methods)),
    %% not enough params
    ?assertNot(pp:allow([contacts1, create], Methods)),

    %% [orders, [get, <<"002">>]]
    ?assert(pp:allow([orders1, get, [<<"001">>]], Methods)),
    %% not in params whitelist
    ?assertNot(pp:allow([orders1, get, [<<"002">>]], Methods)),
    ?assertNot(pp:allow([orders1, send, [<<"001">>, <<"002">>]], Methods)),
    ?assert(pp:allow([orders1, send, [<<"001">>, <<"abc">>]], Methods)).

% en_de_pass_test() ->
%     Str = <<"123456">>,
%     Pass = <<"45,-48,-60,-95,40,126,-44,121,">>,
%     ?assertEqual(Pass, pp_utils:en_pass(Str)),
%     ?assertEqual(Str, pp_utils:de_pass(Pass)).

