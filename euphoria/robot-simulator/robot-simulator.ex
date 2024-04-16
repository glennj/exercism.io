namespace robot

include std/math.e

enum X, Y, DIR

type direction(sequence word)
    switch word do
        case "north", "east", "south", "west" then return 1
        case else return 0
    end switch
end type

type robot(sequence data)
    return
        length(data) = 3 and
        integer(data[X]) and
        integer(data[Y]) and
        direction(data[DIR])
end type

public function move(robot r, sequence commands)
    for i = 1 to length(commands) do
        switch commands[i] do
            case 'R' then r = turn_right(r)
            case 'L' then r = turn_left(r)
            case 'A' then r = advance(r)
        end switch
    end for

    return r
end function

function turn_right(robot r)
    switch r[DIR] do
        case "north" then return {r[X], r[Y], "east"}
        case "east"  then return {r[X], r[Y], "south"}
        case "south" then return {r[X], r[Y], "west"}
        case "west"  then return {r[X], r[Y], "north"}
    end switch
end function

function turn_left(robot r)
    switch r[DIR] do
        case "north" then return {r[X], r[Y], "west"}
        case "east"  then return {r[X], r[Y], "north"}
        case "south" then return {r[X], r[Y], "east"}
        case "west"  then return {r[X], r[Y], "south"}
    end switch
end function

function advance(robot r)
    switch r[DIR] do
        case "north" then return {r[X], r[Y] + 1, r[DIR]}
        case "south" then return {r[X], r[Y] - 1, r[DIR]}
        case "east"  then return {r[X] + 1, r[Y], r[DIR]}
        case "west"  then return {r[X] - 1, r[Y], r[DIR]}
    end switch
end function

