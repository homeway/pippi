-module(users).
-compile(export_all).

create_table() ->
    nosqlite:create_table(?MODULE, ram).

total() ->
    Tab = nosqlite:table(?MODULE),
    length(Tab:all()).

get(_Id) -> none.
