%% -*- mode: nitrogen -*-
-module(pp_convert).

%% API
-export([to_binary/1, to_list/1, to_integer/1, to_atom/1, encode_atom/1]).

-spec to_binary(term()) -> binary().
to_binary(A) when is_atom(A)    -> to_binary(atom_to_list(A));
to_binary(B) when is_binary(B)  -> B;
to_binary(I) when is_integer(I) -> to_binary(integer_to_list(I));
to_binary(L) when is_list(L)    -> unicode:characters_to_binary(L).

-spec to_integer(term()) -> integer().
to_integer(A) when is_atom(A)    -> to_integer(atom_to_list(A));
to_integer(B) when is_binary(B)  -> to_integer(binary_to_list(B));
to_integer(I) when is_integer(I) -> I;
to_integer(L) when is_list(L)    -> list_to_integer(L);
to_integer(F) when is_float(F)   -> round(F).

-spec to_list(term()) -> list().
to_list(A) when is_atom(A)   -> [A];
to_list(B) when is_binary(B) -> [B];
to_list(L) when is_list(L)   -> L;
to_list(T) when is_tuple(T)  -> tuple_to_list(T);
to_list(I) when is_integer(I) -> [I].

-spec to_atom(term()) -> atom().
to_atom(A) when is_atom(A)   -> A;
to_atom(B) when is_binary(B) -> binary_to_atom(B, latin1);
to_atom(L) when is_list(L)   -> list_to_atom(L).

%% 将字符串编码为原子, 主要针对unicode不能作为html_id的情况
encode_atom(S) ->
    Str1 = bitstring_to_list(to_binary(S)),
    Str2 = encode_bin_acc(Str1, ""),
    list_to_atom(lists:reverse(Str2)).
encode_bin_acc([], Acc) ->
    Acc;
encode_bin_acc([Char|Str], Acc) ->
    if
        Char > 195 -> encode_bin_acc(Str, [(Char rem 25 + $A)|Acc]);
        Char > 127 -> encode_bin_acc(Str, [(Char rem 25 + $a)|Acc]);
        true -> encode_bin_acc(Str, [Char|Acc])
    end.
