#!gawk

@include "assert"
@include "arrays"

@namespace "Forth"

BEGIN {
    arrays::init(Stack)
    arrays::init(Defs)
    Count = 0
}

function new() {
    id = ++Count
    arrays::init(Stack, id)
    arrays::init(Defs, id)
    return id
}

function destroy(id) {
    delete Stack[id]
    delete Defs[id]
}

function evaluate(id, line,     n) {
    n = split(line, words, " ")
    if (words[1] == ":")
        RecordDefinition(id, words, n)
    else
        Operation(id, words, n)
}

function toString(id) {
    return arrays::join(Stack[id])
}

############################################################
function RecordDefinition(id, words, n,    name, i, word, defn) {
    awk::assert(words[n] == ";", "macro not terminated with semicolon")
    awk::assert(n > 3, "empty macro definition")
    awk::assert(!LooksLikeANumber(words[2]), "illegal operation")

    name = toupper(words[2])
    arrays::init(defn)
    for (i = 3; i < length(words); i++) {
        word = toupper(words[i])
        arrays::push(defn, word in Defs[id] ? Defs[id][word] : word)
    }
    Defs[id][name] = arrays::join(defn, SUBSEP)
}

function PrependDefinition(id, name, words,    defn) {
    split(Defs[id][name], defn, SUBSEP)
    while (!arrays::isempty(defn))
        arrays::unshift(words, arrays::pop(defn))
}

############################################################
function Operation(id, words, n,    word) {
    while (!arrays::isempty(words)) {
        word = toupper(arrays::shift(words))

        if (word in Defs[id])
            PrependDefinition(id, word, words)
        else {
            switch (word) {
                case "+": case "-": case "*": case "/":
                    Arithmetic(id, word)
                    break
                case "DUP": case "DROP": case "SWAP": case "OVER":
                    @word(id)
                    break
                default:
                    awk::assert(LooksLikeANumber(word), "undefined operation")
                    arrays::push(Stack[id], word)
            }
        }
    }
}

############################################################
function Arithmetic(id, operation,    a, b) {
    RequireSize(id, 2)
    b = arrays::pop(Stack[id])
    a = arrays::pop(Stack[id])
    switch (operation) {
        case "+": arrays::push(Stack[id], a + b); break
        case "-": arrays::push(Stack[id], a - b); break
        case "*": arrays::push(Stack[id], a * b); break
        case "/":
            awk::assert(b != 0, "divide by zero")
            arrays::push(Stack[id], int(a / b)); break
    }
}

function DUP(id) {
    RequireSize(id, 1)
    arrays::push(Stack[id], arrays::peek(Stack[id]))
}

function DROP(id) {
    RequireSize(id, 1)
    arrays::pop(Stack[id])
}

function SWAP(id,    a, b) {
    RequireSize(id, 2)
    b = arrays::pop(Stack[id])
    a = arrays::pop(Stack[id])
    arrays::push(Stack[id], b)
    arrays::push(Stack[id], a)
}

function OVER(id,    a, b) {
    RequireSize(id, 2)
    b = arrays::pop(Stack[id])
    a = arrays::peek(Stack[id])
    arrays::push(Stack[id], b)
    arrays::push(Stack[id], a)
}

############################################################
function LooksLikeANumber(num) {
    return num ~ /^[+-]?[0-9]+$/
}

function RequireSize(id, n,    len) {
    if (n > 0) {
        len = length(Stack[id])
        awk::assert(len != 0, "empty stack")
        awk::assert(len >= n, "only one value on the stack")
    }
}
