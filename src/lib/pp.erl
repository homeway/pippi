%% -*- mode: nitrogen -*-
-module(pp).
-compile(export_all).

%% utils ----------------------------------------------------
to_binary(Term)     -> ss_convert:to_binary(Term).
to_list(Term)       -> ss_convert:to_list(Term).
to_atom(Term)       -> ss_convert:to_atom(Term).

to_string(Term)     -> ss_utils:to_string(Term).
display(Term)       -> erlang:display(ss_utils:to_string(Term)).

uuid()              -> ss_utils:uuid().

now_to_human()      -> ss_time:now_to_human().
now_to_human(Time)  -> ss_time:now_to_human(Time).
