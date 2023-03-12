import Base: show

struct Point{T<:Integer}
    x::T
    y::T
end
show(io::IO, p::Point) = print(io, (p.x, p.y))

const EAST  =   0
const NORTH =  90
const WEST  = 180
const SOUTH = 270

mutable struct Robot
    pos::Point
    head::UInt16

    function Robot(pos::Tuple{Int,Int}, head::Int)
        # TODO validation
        new(Point(pos[1], pos[2]), head)
    end
end

position(robot::Robot) = robot.pos 
heading(robot::Robot)  = robot.head

function show(io::IO, robot::Robot)
    print(io, "Robot(")
    show(io, position(robot))
    dirs = Dict([(0,"EAST"),(90,"NORTH"),(180,"WEST"),(270,"SOUTH")])
    print(io, ", $(dirs[heading(robot)]))")
end

function turn_right!(robot::Robot)
    robot.head = (robot.head - 90 + 360) % 360
    robot
end

function turn_left!(robot::Robot)
    robot.head = (robot.head + 90) % 360
    robot
end

function advance!(robot::Robot)
    rad = heading(robot) * 2π / 360.0
    Δy, Δx = map(x -> round(Int, x), sincos(rad))
    x = position(robot).x + Δx
    y = position(robot).y + Δy
    robot.pos = Point(x, y)
    robot
end

function move!(robot::Robot, script::AbstractString)
    dispatch = Dict([
        ('A', advance!),
        ('R', turn_right!),
        ('L', turn_left!)
    ])
    for i in script
        try
            dispatch[i](robot)
        catch
            throw(ArgumentError(script))
        end
    end
    robot
end
