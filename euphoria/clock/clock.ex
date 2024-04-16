namespace clock

include std/math.e
include std/search.e

constant MINS_PER_HOUR = 60
constant MINS_PER_DAY = 24 * MINS_PER_HOUR


type clock(sequence data)
    return
        length(data) = 1 and
        integer(data[1]) and
        is_in_range(data[1], {0, MINS_PER_DAY}, "[)")
end type


public function create(integer hours, integer minutes)
    integer normalized = mod(hours * MINS_PER_HOUR + minutes, MINS_PER_DAY)
    return {normalized}
end function


public function add(clock c, integer minutes)
    return create(0, c[1] + minutes)
end function

public function subtract(clock c, integer minutes)
    return add(c, -minutes)
end function


public function toString(clock c)
    integer hour = trunc(c[1] / MINS_PER_HOUR)
    integer mins = mod(c[1], MINS_PER_HOUR)
    return sprintf("%02d:%02d", {hour, mins})
end function


public function equalClocks(clock c1, clock c2)
    return c1[1] = c2[1]
end function
