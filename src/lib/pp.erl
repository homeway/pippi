%% -*- mode: nitrogen -*-
-module(pp).
-compile(export_all).

%% utils ----------------------------------------------------
to_binary(Term)     -> pp_convert:to_binary(Term).
to_list(Term)       -> pp_convert:to_list(Term).
to_atom(Term)       -> pp_convert:to_atom(Term).

to_string(Term)     -> pp_utils:to_string(Term).
display(Term)       -> erlang:display(pp_utils:to_string(Term)).

uuid()              -> pp_utils:uuid().

now_to_human()      -> pp_time:now_to_human().
now_to_human(Time)  -> pp_time:now_to_human(Time).
