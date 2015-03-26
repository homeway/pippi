%% -*- mode: nitrogen -*-
-module(pp_utils).
-export([uuid/0]).
-export([to_string/1, apply/3, allow/2]).

%% to replace erlang:display/1
to_string(L) -> lists:flatten(to_string(L, "")).

to_string([], Acc0) ->
    Acc0 ++ "[]";
to_string(L, Acc0) when is_list(L) ->
    IsString = lists:all(fun(I) ->
        is_integer(I) and (I =< 126) and (I >= 32)
    end, L),
    case IsString of
        true ->
            Acc0 ++ "\"" ++ L ++ "\"";
        false ->
            L2 = lists:map(fun(I) ->
                to_string(I)
            end, L),
            Acc0 ++ "[" ++ string:join(L2, ", ") ++ "]"
    end;

to_string(M, Acc0) when is_map(M) ->
    L2 = lists:map(fun({K, V}) ->
        to_string(K) ++ " => " ++ to_string(V)
    end, maps:to_list(M)),
    Acc0 ++ "#{" ++ string:join(L2, ", ") ++ "}";

to_string(T, Acc0) when is_tuple(T) ->
    L2 = [to_string(I) || I <- tuple_to_list(T)],
    Acc0 ++ "{" ++ string:join(L2, ", ") ++ "}";

to_string(B, Acc0) when is_binary(B) ->
    Acc0 ++ io_lib:format("<<\"~ts\">>", [B]);

to_string(I, Acc0) when is_integer(I) ->
    Acc0 ++ io_lib:format("~B", [I]);

to_string(Term, Acc0) ->
    Acc0 ++ io_lib:format("~p", [Term]).

uuid() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:rand_bytes(16),
    Str = io_lib:format("~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b", 
                        [A, B, C band 16#0fff, D band 16#3fff bor 16#8000, E]),
    list_to_binary(Str).

%% support nosqlite methods apply
apply([nosqlite, M0], F0, A) ->
    M = pp:to_atom(M0), F = pp:to_atom(F0),
    code:ensure_loaded(M),
    case erlang:function_exported(M, F, length(A)) of
        true -> erlang:apply(M, F, A);
        _ -> erlang:apply({nosqlite, M}, F, A)
    end;
apply(M, F, A) -> erlang:apply(M, F, A).

%% allow to use methods

%% nosqlite
allow([[nosqlite, M], F], Methods) -> allow([M, F], Methods);
allow([[nosqlite, M], F, A], Methods) -> allow([M, F, A], Methods);

%% no arguments
allow([M, F], Methods) -> allow([M, F, []], Methods);

%% tuple module
allow([[M|A1], F, A0], Methods) -> allow([M, F, A0++A1], Methods);

%% common mfa
allow([M, F, A], Methods) ->
    lists:any(fun(I) -> allow_item([M, F, A], I) end, Methods).

allow_item([M, _F, _A], M) -> true;
allow_item([M, F, _A], [M, F]) -> true;
allow_item([M, F, A], [M, Funs]) when is_list(Funs) ->
    lists:any(fun(Fun) ->
        if
            is_atom(Fun) -> F =:= Fun;
            is_list(Fun) -> [F|A] =:= Fun;
            true -> false
        end
    end, Funs);
allow_item(_, _) -> false.
