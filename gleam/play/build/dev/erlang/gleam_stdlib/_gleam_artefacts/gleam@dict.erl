-module(gleam@dict).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([size/1, to_list/1, new/0, get/2, has_key/2, insert/3, from_list/1, keys/1, values/1, take/2, merge/2, delete/2, drop/2, update/3, fold/3, map_values/2, filter/2]).
-export_type([dict/2]).

-type dict(KF, KG) :: any() | {gleam_phantom, KF, KG}.

-spec size(dict(any(), any())) -> integer().
size(Dict) ->
    maps:size(Dict).

-spec to_list(dict(KL, KM)) -> list({KL, KM}).
to_list(Dict) ->
    maps:to_list(Dict).

-spec new() -> dict(any(), any()).
new() ->
    maps:new().

-spec get(dict(LS, LT), LS) -> {ok, LT} | {error, nil}.
get(From, Get) ->
    gleam_stdlib:map_get(From, Get).

-spec has_key(dict(LC, any()), LC) -> boolean().
has_key(Dict, Key) ->
    maps:is_key(Key, Dict).

-spec insert(dict(ME, MF), ME, MF) -> dict(ME, MF).
insert(Dict, Key, Value) ->
    maps:put(Key, Value, Dict).

-spec fold_list_of_pair(list({KV, KW}), dict(KV, KW)) -> dict(KV, KW).
fold_list_of_pair(List, Initial) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold_list_of_pair(
                Rest,
                insert(Initial, erlang:element(1, X), erlang:element(2, X))
            )
    end.

-spec from_list(list({KQ, KR})) -> dict(KQ, KR).
from_list(List) ->
    maps:from_list(List).

-spec reverse_and_concat(list(TF), list(TF)) -> list(TF).
reverse_and_concat(Remaining, Accumulator) ->
    case Remaining of
        [] ->
            Accumulator;

        [Item | Rest] ->
            reverse_and_concat(Rest, [Item | Accumulator])
    end.

-spec do_keys_acc(list({NR, any()}), list(NR)) -> list(NR).
do_keys_acc(List, Acc) ->
    case List of
        [] ->
            reverse_and_concat(Acc, []);

        [X | Xs] ->
            do_keys_acc(Xs, [erlang:element(1, X) | Acc])
    end.

-spec keys(dict(NE, any())) -> list(NE).
keys(Dict) ->
    maps:keys(Dict).

-spec do_values_acc(list({any(), OH}), list(OH)) -> list(OH).
do_values_acc(List, Acc) ->
    case List of
        [] ->
            reverse_and_concat(Acc, []);

        [X | Xs] ->
            do_values_acc(Xs, [erlang:element(2, X) | Acc])
    end.

-spec values(dict(any(), NX)) -> list(NX).
values(Dict) ->
    maps:values(Dict).

-spec insert_taken(dict(PL, PM), list(PL), dict(PL, PM)) -> dict(PL, PM).
insert_taken(Dict, Desired_keys, Acc) ->
    Insert = fun(Taken, Key) -> case get(Dict, Key) of
            {ok, Value} ->
                insert(Taken, Key, Value);

            _ ->
                Taken
        end end,
    case Desired_keys of
        [] ->
            Acc;

        [X | Xs] ->
            insert_taken(Dict, Xs, Insert(Acc, X))
    end.

-spec take(dict(OX, OY), list(OX)) -> dict(OX, OY).
take(Dict, Desired_keys) ->
    maps:with(Desired_keys, Dict).

-spec insert_pair(dict(QK, QL), {QK, QL}) -> dict(QK, QL).
insert_pair(Dict, Pair) ->
    insert(Dict, erlang:element(1, Pair), erlang:element(2, Pair)).

-spec fold_inserts(list({QQ, QR}), dict(QQ, QR)) -> dict(QQ, QR).
fold_inserts(New_entries, Dict) ->
    case New_entries of
        [] ->
            Dict;

        [X | Xs] ->
            fold_inserts(Xs, insert_pair(Dict, X))
    end.

-spec merge(dict(PU, PV), dict(PU, PV)) -> dict(PU, PV).
merge(Dict, New_entries) ->
    maps:merge(Dict, New_entries).

-spec delete(dict(QX, QY), QX) -> dict(QX, QY).
delete(Dict, Key) ->
    maps:remove(Key, Dict).

-spec drop(dict(RJ, RK), list(RJ)) -> dict(RJ, RK).
drop(Dict, Disallowed_keys) ->
    case Disallowed_keys of
        [] ->
            Dict;

        [X | Xs] ->
            drop(delete(Dict, X), Xs)
    end.

-spec update(dict(RQ, RR), RQ, fun((gleam@option:option(RR)) -> RR)) -> dict(RQ, RR).
update(Dict, Key, Fun) ->
    _pipe = Dict,
    _pipe@1 = get(_pipe, Key),
    _pipe@2 = gleam@option:from_result(_pipe@1),
    _pipe@3 = Fun(_pipe@2),
    insert(Dict, Key, _pipe@3).

-spec do_fold(list({RX, RY}), SA, fun((SA, RX, RY) -> SA)) -> SA.
do_fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Rest] ->
            do_fold(Rest, Fun(Initial, K, V), Fun)
    end.

-spec fold(dict(SB, SC), SF, fun((SF, SB, SC) -> SF)) -> SF.
fold(Dict, Initial, Fun) ->
    _pipe = Dict,
    _pipe@1 = maps:to_list(_pipe),
    do_fold(_pipe@1, Initial, Fun).

-spec map_values(dict(MQ, MR), fun((MQ, MR) -> MU)) -> dict(MQ, MU).
map_values(Dict, Fun) ->
    maps:map(Fun, Dict).

-spec filter(dict(OL, OM), fun((OL, OM) -> boolean())) -> dict(OL, OM).
filter(Dict, Predicate) ->
    maps:filter(Predicate, Dict).
