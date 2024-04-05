include std/sort.e
include std/sequence.e

public function is_equilateral(sequence sides)
    return is_valid(sides) and count(sides) = 1
end function

public function is_isosceles(sequence sides)
    return is_valid(sides) and count(sides) <= 2
end function

public function is_scalene(sequence sides)
    return is_valid(sides) and count(sides) = 3
end function

  
function is_valid(sequence sides)
    sequence s = sort(sides)
    return s[1] > 0 and s[1] + s[2] > s[3]
end function

function count(sequence sides)
    return length(remove_dups(sides, RD_SORT))
end function
