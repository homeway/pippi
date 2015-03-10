%% -*- mode: nitrogen -*-
-module(ss_time).
-export([iso_to_human/1, iso_to_human/2, human_to_iso/1]).
-export([now_to_iso/0, now_to_human/0, now_to_human/1]).

%% 从iso格式<<"2014-01-23T12:23:01Z">>转为{{y,m,d},{h,mi,s}}格式
iso_to_dt(<< Y:4/binary, "-", M:2/binary, "-", D:2/binary, "T",
             H:2/binary, ":", Mi:2/binary, ":", S:2/binary, "Z">>) ->
    {{binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)},
     {binary_to_integer(H), binary_to_integer(Mi), binary_to_integer(S)}};
iso_to_dt(_) ->{{2014,1,1}, {0,0,0}}.

%% 从可读格式<<"2014-01-23 12:23:34">>转为{{y,m,d},{h,mi,s}}格式
human_to_dt(<< Y:4/binary, "-", M:2/binary, "-", D:2/binary, " ",
               H:2/binary, ":", Mi:2/binary, ":", S:2/binary >>) ->
    {{binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)},
     {binary_to_integer(H), binary_to_integer(Mi), binary_to_integer(S)}};
human_to_dt(<< Y:4/binary, "-", M:2/binary, "-", D:2/binary >>) ->
    {{binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)},
     {0, 0, 0}};
human_to_dt(_) ->
    {{2014, 1, 1},{0, 0, 0}}.

%% universal转为local时间 calendar:universal_time_to_local_time/1
u2l_list(UniTuple) ->
    {{Y, M, D}, {H, Mi, S}} = calendar:universal_time_to_local_time(UniTuple),
    [Y, M, D, H, Mi, S].
%% local转为universal时间 calendar:local_time_to_universal_time/1
l2u_list(LocalTuple) ->
    {{Y, M, D}, {H, Mi, S}} = calendar:local_time_to_universal_time(LocalTuple),
    [Y, M, D, H, Mi, S].

%% 从Iso存储格式转为可读格式
iso_to_human(Datetime) ->
    iso_to_human(datetime, Datetime).
iso_to_human(datetime, Datetime) ->
    ss:to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", u2l_list(iso_to_dt(Datetime))));
iso_to_human(date, Datetime) ->
    [Y, M, D|_] = u2l_list(iso_to_dt(Datetime)),
    ss:to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])).

%% 从可读格式转存为Iso格式
human_to_iso(Datetime1) ->
    Datetime = human_to_dt(ss:to_binary(Datetime1)),
    ss:to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", l2u_list(Datetime))).

%% 当前iso时间串
now_to_iso() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:now_to_universal_time(erlang:now()),
    ss:to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Y, M, D, H, Mi, S])).
%% 当前local时间串
now_to_human() -> now_to_human(erlang:now()).
now_to_human(Time) ->
    {{Y, M, D}, {H, Mi, S}} = calendar:now_to_local_time(Time),
    ss:to_binary(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Y, M, D, H, Mi, S])).
