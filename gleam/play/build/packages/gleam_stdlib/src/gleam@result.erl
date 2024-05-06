-module(gleam@result).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, 'try'/2, then/2, unwrap/2, lazy_unwrap/2, unwrap_error/2, unwrap_both/1, nil_error/1, 'or'/2, lazy_or/2, all/1, partition/1, replace/2, replace_error/2, values/1, try_recover/2]).

-spec is_ok({ok, any()} | {error, any()}) -> boolean().
is_ok(Result) ->
    case Result of
        {error, _} ->
            false;

        {ok, _} ->
            true
    end.

-spec is_error({ok, any()} | {error, any()}) -> boolean().
is_error(Result) ->
    case Result of
        {ok, _} ->
            false;

        {error, _} ->
            true
    end.

-spec map({ok, BJG} | {error, BJH}, fun((BJG) -> BJK)) -> {ok, BJK} |
    {error, BJH}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-spec map_error({ok, BJN} | {error, BJO}, fun((BJO) -> BJR)) -> {ok, BJN} |
    {error, BJR}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-spec flatten({ok, {ok, BJU} | {error, BJV}} | {error, BJV}) -> {ok, BJU} |
    {error, BJV}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-spec 'try'({ok, BKC} | {error, BKD}, fun((BKC) -> {ok, BKG} | {error, BKD})) -> {ok,
        BKG} |
    {error, BKD}.
'try'(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-spec then({ok, BKL} | {error, BKM}, fun((BKL) -> {ok, BKP} | {error, BKM})) -> {ok,
        BKP} |
    {error, BKM}.
then(Result, Fun) ->
    'try'(Result, Fun).

-spec unwrap({ok, BKU} | {error, any()}, BKU) -> BKU.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-spec lazy_unwrap({ok, BKY} | {error, any()}, fun(() -> BKY)) -> BKY.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default()
    end.

-spec unwrap_error({ok, any()} | {error, BLD}, BLD) -> BLD.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _} ->
            Default;

        {error, E} ->
            E
    end.

-spec unwrap_both({ok, BLG} | {error, BLG}) -> BLG.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-spec nil_error({ok, BLJ} | {error, any()}) -> {ok, BLJ} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-spec 'or'({ok, BLP} | {error, BLQ}, {ok, BLP} | {error, BLQ}) -> {ok, BLP} |
    {error, BLQ}.
'or'(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second
    end.

-spec lazy_or({ok, BLX} | {error, BLY}, fun(() -> {ok, BLX} | {error, BLY})) -> {ok,
        BLX} |
    {error, BLY}.
lazy_or(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second()
    end.

-spec all(list({ok, BMF} | {error, BMG})) -> {ok, list(BMF)} | {error, BMG}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-spec do_partition(list({ok, BMU} | {error, BMV}), list(BMU), list(BMV)) -> {list(BMU),
    list(BMV)}.
do_partition(Results, Oks, Errors) ->
    case Results of
        [] ->
            {Oks, Errors};

        [{ok, A} | Rest] ->
            do_partition(Rest, [A | Oks], Errors);

        [{error, E} | Rest@1] ->
            do_partition(Rest@1, Oks, [E | Errors])
    end.

-spec partition(list({ok, BMN} | {error, BMO})) -> {list(BMN), list(BMO)}.
partition(Results) ->
    do_partition(Results, [], []).

-spec replace({ok, any()} | {error, BND}, BNG) -> {ok, BNG} | {error, BND}.
replace(Result, Value) ->
    case Result of
        {ok, _} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-spec replace_error({ok, BNJ} | {error, any()}, BNN) -> {ok, BNJ} | {error, BNN}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _} ->
            {error, Error}
    end.

-spec values(list({ok, BNQ} | {error, any()})) -> list(BNQ).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).

-spec try_recover(
    {ok, BNW} | {error, BNX},
    fun((BNX) -> {ok, BNW} | {error, BOA})
) -> {ok, BNW} | {error, BOA}.
try_recover(Result, Fun) ->
    case Result of
        {ok, Value} ->
            {ok, Value};

        {error, Error} ->
            Fun(Error)
    end.
