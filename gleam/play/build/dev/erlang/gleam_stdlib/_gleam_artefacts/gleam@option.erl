-module(gleam@option).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([all/1, is_some/1, is_none/1, to_result/2, from_result/1, unwrap/2, lazy_unwrap/2, map/2, flatten/1, then/2, 'or'/2, lazy_or/2, values/1]).
-export_type([option/1]).

-type option(FO) :: {some, FO} | none.

-spec do_all(list(option(FP)), list(FP)) -> option(list(FP)).
do_all(List, Acc) ->
    case List of
        [] ->
            {some, Acc};

        [X | Rest] ->
            Accumulate = fun(Acc@1, Item) -> case {Acc@1, Item} of
                    {{some, Values}, {some, Value}} ->
                        {some, [Value | Values]};

                    {_, _} ->
                        none
                end end,
            Accumulate(do_all(Rest, Acc), X)
    end.

-spec all(list(option(FV))) -> option(list(FV)).
all(List) ->
    do_all(List, []).

-spec is_some(option(any())) -> boolean().
is_some(Option) ->
    Option /= none.

-spec is_none(option(any())) -> boolean().
is_none(Option) ->
    Option =:= none.

-spec to_result(option(GE), GH) -> {ok, GE} | {error, GH}.
to_result(Option, E) ->
    case Option of
        {some, A} ->
            {ok, A};

        _ ->
            {error, E}
    end.

-spec from_result({ok, GK} | {error, any()}) -> option(GK).
from_result(Result) ->
    case Result of
        {ok, A} ->
            {some, A};

        _ ->
            none
    end.

-spec unwrap(option(GP), GP) -> GP.
unwrap(Option, Default) ->
    case Option of
        {some, X} ->
            X;

        none ->
            Default
    end.

-spec lazy_unwrap(option(GR), fun(() -> GR)) -> GR.
lazy_unwrap(Option, Default) ->
    case Option of
        {some, X} ->
            X;

        none ->
            Default()
    end.

-spec map(option(GT), fun((GT) -> GV)) -> option(GV).
map(Option, Fun) ->
    case Option of
        {some, X} ->
            {some, Fun(X)};

        none ->
            none
    end.

-spec flatten(option(option(GX))) -> option(GX).
flatten(Option) ->
    case Option of
        {some, X} ->
            X;

        none ->
            none
    end.

-spec then(option(HB), fun((HB) -> option(HD))) -> option(HD).
then(Option, Fun) ->
    case Option of
        {some, X} ->
            Fun(X);

        none ->
            none
    end.

-spec 'or'(option(HG), option(HG)) -> option(HG).
'or'(First, Second) ->
    case First of
        {some, _} ->
            First;

        none ->
            Second
    end.

-spec lazy_or(option(HK), fun(() -> option(HK))) -> option(HK).
lazy_or(First, Second) ->
    case First of
        {some, _} ->
            First;

        none ->
            Second()
    end.

-spec do_values(list(option(HO)), list(HO)) -> list(HO).
do_values(List, Acc) ->
    case List of
        [] ->
            Acc;

        [X | Xs] ->
            Accumulate = fun(Acc@1, Item) -> case Item of
                    {some, Value} ->
                        [Value | Acc@1];

                    none ->
                        Acc@1
                end end,
            Accumulate(do_values(Xs, Acc), X)
    end.

-spec values(list(option(HT))) -> list(HT).
values(Options) ->
    do_values(Options, []).
