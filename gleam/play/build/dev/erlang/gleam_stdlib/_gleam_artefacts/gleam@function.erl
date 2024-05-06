-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-spec compose(fun((EPG) -> EPH), fun((EPH) -> EPI)) -> fun((EPG) -> EPI).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-spec curry2(fun((EPJ, EPK) -> EPL)) -> fun((EPJ) -> fun((EPK) -> EPL)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-spec curry3(fun((EPN, EPO, EPP) -> EPQ)) -> fun((EPN) -> fun((EPO) -> fun((EPP) -> EPQ))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-spec curry4(fun((EPS, EPT, EPU, EPV) -> EPW)) -> fun((EPS) -> fun((EPT) -> fun((EPU) -> fun((EPV) -> EPW)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-spec curry5(fun((EPY, EPZ, EQA, EQB, EQC) -> EQD)) -> fun((EPY) -> fun((EPZ) -> fun((EQA) -> fun((EQB) -> fun((EQC) -> EQD))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-spec curry6(fun((EQF, EQG, EQH, EQI, EQJ, EQK) -> EQL)) -> fun((EQF) -> fun((EQG) -> fun((EQH) -> fun((EQI) -> fun((EQJ) -> fun((EQK) -> EQL)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-spec flip(fun((EQN, EQO) -> EQP)) -> fun((EQO, EQN) -> EQP).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-spec identity(EQQ) -> EQQ.
identity(X) ->
    X.

-spec constant(EQR) -> fun((any()) -> EQR).
constant(Value) ->
    fun(_) -> Value end.

-spec tap(EQT, fun((EQT) -> any())) -> EQT.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-spec apply1(fun((EQV) -> EQW), EQV) -> EQW.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-spec apply2(fun((EQX, EQY) -> EQZ), EQX, EQY) -> EQZ.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-spec apply3(fun((ERA, ERB, ERC) -> ERD), ERA, ERB, ERC) -> ERD.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
