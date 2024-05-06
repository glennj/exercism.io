-module(play_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0, hello_world_test/0]).

-spec main() -> nil.
main() ->
    gleeunit:main().

-spec hello_world_test() -> nil.
hello_world_test() ->
    _pipe = 1,
    gleeunit_ffi:should_equal(_pipe, 1).
