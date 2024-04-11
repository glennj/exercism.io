-- let's do a recursive function
public function reverse(sequence str, sequence reversed = "")
    if length(str) = 0 then
        return reversed
    else
        return reverse(str[2..$], str[1] & reversed)
    end if
end function
