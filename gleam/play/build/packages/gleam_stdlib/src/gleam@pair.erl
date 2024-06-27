-module(gleam@pair).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([first/1, second/1, swap/1, map_first/2, map_second/2, new/2]).

-spec first({XD, any()}) -> XD.
first(Pair) ->
    {A, _} = Pair,
    A.

-spec second({any(), XG}) -> XG.
second(Pair) ->
    {_, A} = Pair,
    A.

-spec swap({XH, XI}) -> {XI, XH}.
swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

-spec map_first({XJ, XK}, fun((XJ) -> XL)) -> {XL, XK}.
map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

-spec map_second({XM, XN}, fun((XN) -> XO)) -> {XM, XO}.
map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.

-spec new(XP, XQ) -> {XP, XQ}.
new(First, Second) ->
    {First, Second}.
