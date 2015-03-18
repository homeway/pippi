%% -*- mode: nitrogen -*-
-module(mysync).
-export([go/0, stop/0]).

go() ->
    sync:go(),
    application:start(snowstorm),
    application:start(sample),
    RunTests = fun(Mods) ->
        ToTest1 = [Mod || Mod <- Mods, erlang:function_exported(Mod, test, 0)],
        ToTest2 = lists:filtermap(fun(M) ->
            M2 = list_to_atom(atom_to_list(M) ++ "_test"),
            code:ensure_loaded(M2),
            case erlang:function_exported(M2, test, 0) of
                true -> {true, M2};
                _ -> false
            end
        end, Mods),
        lists:map(fun(M) ->
            case M:test() of
                ok ->
                    sync_notify:growl_success("test ok");
                _ ->
                    sync_notify:growl_errors("test failed")
            end
        end, lists:flatten(ToTest1 ++ ToTest2))
    end,
    sync:onsync(RunTests).

stop() -> sync:stop().