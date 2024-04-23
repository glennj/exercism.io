include std/search.e
include std/math.e

constant SIZE = 8
enum RANK, FILE

public type queen(sequence position)
    return length(position) = 2
        and is_in_range(position[RANK], {0, SIZE}, "[)")
        and is_in_range(position[FILE], {0, SIZE}, "[)")
end type

public function can_attack(queen q1, queen q2)
    return q1[RANK] = q2[RANK]
        or q1[FILE] = q2[FILE]
        or abs(q1[RANK] - q2[RANK]) = abs(q1[FILE] - q2[FILE])
end function
