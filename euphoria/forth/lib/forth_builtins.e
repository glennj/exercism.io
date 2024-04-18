include std/map.e
include std/stack.e
include lib/forth_types.e
 
public function init_builtins()
    map w = map:new()
    put(w, "drop", routine_id("drop"))
    put(w, "dup", routine_id("dup"))
    put(w, "swap", routine_id("swap"))
    put(w, "over", routine_id("over"))
    put(w, "+", routine_id("add"))
    put(w, "-", routine_id("sub"))
    put(w, "*", routine_id("mul"))
    put(w, "/", routine_id("div"))
    return w
end function

function need(stack s, integer quantity)
    integer sz = stack:size(s)
    if sz < quantity then
        switch sz do
            case 0 then return {ERR, "empty stack"}
            case 1 then return {ERR, "only one value on the stack"}
            case else   return {ERR, "not enough values on the stack"}
        end switch
    end if
    return {OK}
end function

function drop(stack s)
    result res = need(s, 1)
    if error(res) then return res end if
    pop(s)
    return {OK}
end function

function dup(stack s)
    result res = need(s, 1)
    if error(res) then return res end if
    object a = top(s)
    push(s, a)
    return {OK}
end function

function swap(stack s)
    result res = need(s, 2)
    if error(res) then return res end if
    object b = pop(s)
    object a = pop(s)
    push(s, b)
    push(s, a)
    return {OK}
end function

function over(stack s)
    result res = need(s, 2)
    if error(res) then return res end if
    object b = pop(s)
    object a = top(s)
    push(s, b)
    push(s, a)
    return {OK}
end function

function add(stack s)
    result res = need(s, 2)
    if error(res) then return res end if
    object b = pop(s)
    object a = pop(s)
    push(s, a + b)
    return {OK}
end function

function sub(stack s)
    result res = need(s, 2)
    if error(res) then return res end if
    object b = pop(s)
    object a = pop(s)
    push(s, a - b)
    return {OK}
end function

function mul(stack s)
    result res = need(s, 2)
    if error(res) then return res end if
    object b = pop(s)
    object a = pop(s)
    push(s, a * b)
    return {OK}
end function

function div(stack s)
    result res = need(s, 2)
    if error(res) then return res end if
    object b = pop(s)
    object a = pop(s)
    if b = 0 then
        return {ERR, "divide by zero"}
    end if
    push(s, floor(a / b))
    return {OK}
end function

