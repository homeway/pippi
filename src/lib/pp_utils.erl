-module(pp_utils).
-export([uuid/0]).
-export([to_string/1]).

%% to replace erlang:display/1
to_string(L) -> lists:flatten(to_string(L, "")).

to_string(L, Acc0) when is_list(L) ->
    IsString = lists:all(fun(I) -> is_integer(I) end, L),
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