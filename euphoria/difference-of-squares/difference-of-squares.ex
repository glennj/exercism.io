-- A functional approach

include std/math.e
include std/sequence.e


function identity(atom num, object _ = {})
    return num
end function

function square(atom num, object _ = {})
    return num * num
end function

integer ID = routine_id("identity")
integer SQ = routine_id("square")


function doIt(atom num, integer innerFunc, integer outerFunc)
    return call_func(outerFunc, {sum(apply(series(1, 1, num), innerFunc, {})), {}})
end function


public function squareOfSum(atom n)
    return doIt(n, ID, SQ)
end function

public function sumOfSquares(atom n)
    return doIt(n, SQ, ID)
end function


public function differenceOfSquares(atom n)
    return abs(sumOfSquares(n) - squareOfSum(n))
end function
