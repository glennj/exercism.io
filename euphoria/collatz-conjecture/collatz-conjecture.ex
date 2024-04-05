include std/math.e

public function steps(integer n, integer count = 0)
    if n < 1 then
        return "Only positive numbers are allowed"
    elsif n = 1 then
        return count
    elsif is_even(n) then
        return steps(n / 2, count + 1)
    else
        return steps(3 * n + 1, count + 1)
    end if
end function
