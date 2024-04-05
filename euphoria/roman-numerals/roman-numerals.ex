/* -- a straightforward series of tests
 *
    public function roman(integer val) 
        sequence r = ""
        while val >= 1000 do   r &=  "M" val -= 1000 end while
        if    val >=  900 then r &= "CM" val -=  900 end if
        if    val >=  500 then r &=  "D" val -=  500 end if
        if    val >=  400 then r &= "CD" val -=  400 end if
        while val >=  100 do   r &=  "C" val -=  100 end while
        if    val >=   90 then r &= "XC" val -=   90 end if
        if    val >=   50 then r &=  "L" val -=   50 end if
        if    val >=   40 then r &= "XL" val -=   40 end if
        while val >=   10 do   r &=  "X" val -=   10 end while
        if    val >=    9 then r &= "IX" val -=    9 end if
        if    val >=    5 then r &=  "V" val -=    5 end if
        if    val >=    4 then r &= "IV" val -=    4 end if
        while val >=    1 do   r &=  "I" val -=    1 end while
        return r
    end function
*/

/* a recursive solution discussed in the Jeremy/Erik video
 */
public function roman(integer val) 
    return helper(val, "")
end function

function helper(integer n, sequence roman)
    if    n >= 400 then return appender(n, roman, 100, 'M', 'D', 'C')
    elsif n >=  40 then return appender(n, roman, 10, 'C', 'L', 'X')
    elsif n >=   1 then return appender(n, roman, 1, 'X', 'V', 'I')
    else                return roman
    end if
end function


function appender(integer n, sequence r, integer base, atom tens, atom fives, atom ones)
    if    n >= 10 * base then return helper(n - 10 * base, r & tens)
    elsif n >=  9 * base then return helper(n + base, r & ones)
    elsif n >=  5 * base then return helper(n - 5 * base, r & fives)
    elsif n >=  4 * base then return helper(n + base, r & ones)
    else                      return helper(n - base, r & ones)
    end if
end function
