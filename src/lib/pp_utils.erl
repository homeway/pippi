%% -*- mode: nitrogen -*-
-module(pp_utils).
-export([uuid/0]).
-export([to_string/1, allow/2, confirm_json/1]).

%% to replace erlang:display/1
to_string(L) -> lists:flatten(to_string(L, "")).

to_string([], Acc0) ->
    Acc0 ++ "[]";
to_string(L, Acc0) when is_list(L) ->
    IsString = lists:all(fun(I) ->
        %% [9] is \t, [10] is \n
        is_integer(I) and ((I =< 126) and (I >= 9))
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
    Str = io_lib:format("~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b", [A, B, C band 16#0fff, D band 16#3fff bor 16#8000, E]),
    list_to_binary(Str).

%% allow to use methods
%%
%% no arguments
allow([M, F], Methods) -> allow([M, F, []], Methods);

%% nosqlite mfa
allow([{nosqlite, M}, F, A], Methods) ->
    code:ensure_loaded(nosqlite),
    case erlang:function_exported(nosqlite, F, length(A) + 1) of
        true ->
            lists:any(fun(I) -> allow_item([M, F, A], I) end, Methods);
        false -> false
    end;
%% common mfa
allow([M, F, A], Methods) ->
    code:ensure_loaded(M),
    case erlang:function_exported(M, F, length(A)) of
        true ->
            lists:any(fun(I) ->
                allow_item([M, F, A], I)
            end, Methods);
        false -> false
    end.

allow_item([M, _F, _A], M) -> true;
allow_item([M, F, _A], [M, F]) -> true;
allow_item([M, F, A], [M, Funs]) when is_list(Funs) ->
    lists:any(fun(Fun) ->
        if
            is_atom(Fun) -> F =:= Fun;
            is_binary(Fun) -> F =:= Fun;
            is_list(Fun) -> [F|A] =:= Fun;
            true -> false
        end
    end, Funs);
allow_item(_, _) -> false.

%% confirm data input compatible with json
confirm_json(Tuple) when is_tuple(Tuple) ->
    confirm_json(erlang:tuple_to_list(Tuple));
confirm_json(Data) ->
    D1 = jiffy:encode(Data),
    jiffy:decode(D1, [return_maps]).
