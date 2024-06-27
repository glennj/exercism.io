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

-spec main() -> list(gleam@regex:match()).
main() ->
    _assert_subject = gleam@regex:from_string(<<"(^)(xr.*|yt.*)"/utf8>>),
    {ok, Re} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"play"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 13})
    end,
    gleam@io:debug(factorial(10)),
    gleam@io:debug(erlang:length(gleam@list:permutations([1, 2, 3, 4, 5]))),
    gleam@io:debug(gleam@regex:scan(Re, <<"xray"/utf8>>)).
