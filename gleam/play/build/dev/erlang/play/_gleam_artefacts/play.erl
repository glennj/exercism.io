-module(play).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).

-spec factorial(integer()) -> integer().
factorial(N) ->
    case N of
        1 ->
            1;

        _ ->
            N * factorial(N - 1)
    end.

-spec main() -> integer().
main() ->
    gleam@io:debug(factorial(1)),
    gleam@io:debug(factorial(2)),
    gleam@io:debug(factorial(3)),
    gleam@io:debug(factorial(4)),
    gleam@io:debug(factorial(5)),
    gleam@io:debug(factorial(6)),
    gleam@io:debug(factorial(7)),
    gleam@io:debug(factorial(8)),
    gleam@io:debug(factorial(9)),
    gleam@io:debug(factorial(10)).
