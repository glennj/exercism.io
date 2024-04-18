include std/convert.e       -- to_integer()
include std/map.e
include std/sequence.e      -- stdseq:split()
include std/stack.e
include std/text.e          -- lower()
include std/types.e         -- sequence_array()
include std/utils.e         -- iif()

include lib/forth_types.e
include lib/forth_builtins.e

enum STACK, BUILTINS, WORDS

public function evaluate(sequence instructions) 
    sequence forth = {
        stack:new(),
        init_builtins(),
        map:new()
    }

    if not sequence_array(instructions) then
        instructions = {instructions}
    end if

    for i = 1 to length(instructions) do
        result res = evaluate_line(forth, lower(instructions[i]))
        if error(res) then
            return errmsg(res)
        end if
    end for

    return reverse(stack_to_sequence(forth[STACK]))
end function

function evaluate_line(sequence forth, sequence instruction)
    sequence tokens = stdseq:split(instruction)
    while length(tokens) > 0 do
        sequence token = tokens[1]
        tokens = tokens[2..$]

        if equal(token, ":") then
            result res = add_word(forth[WORDS], tokens)
            if error(res) then
                return res
            end if
            tokens = result_value(res)

        elsif has(forth[WORDS], token) then
            -- user defined words take precedence over builtins
            tokens = get(forth[WORDS], token) & tokens

        elsif has(forth[BUILTINS], token) then
            integer f = get(forth[BUILTINS], token)
            result res = call_func(f, {forth[STACK]})
            if error(res) then
                return res
            end if

        else
            result res = strtoi(token)
            if error(res) then
                return {ERR, "undefined operation"}
            end if
            push(forth[STACK], result_value(res))
        end if
    end while
    return {OK}
end function

function strtoi(sequence string)
    sequence result = to_number(string, 1)
    return iif(result[2] = 0, {OK, result[1]}, {ERR, "not a number"})
end function

function add_word(map words, sequence tokens)
    integer idx = find(";", tokens)
    if idx = 0 then
        -- TODO look for semicolon on a subsequent input line
        return {ERR, "malformed word definition"}
    end if

    sequence body = tokens[1..idx-1]
    tokens = iif(idx = length(tokens), {}, tokens[idx+1..$])

    sequence name = body[1]
    result res = strtoi(name)
    if ok(res) then
        return {ERR, "illegal operation"}
    end if

    sequence definition = {}
    for i = 2 to length(body) do
        definition = iif(has(words, body[i]),
                         definition & get(words, body[i]),
                         append(definition, body[i]))
    end for
    put(words, name, definition)

    return {OK, tokens}
end function

-- How is this not part of the stack module?
function stack_to_sequence(stack s)
    sequence seq = {}
    while not stack:is_empty(s) do
        seq = append(seq, pop(s))
    end while
    return seq
end function
