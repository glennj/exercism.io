module RobotNames
    using Random

    export name
    _names = Set()

    function randname()
        local name
        while true
            name = randstring('A':'Z', 2) * randstring('0':'9', 3)
            name in _names || break
        end
        push!(_names, name)
        return name
    end
end


using .RobotNames: randname


mutable struct Robot
    name::AbstractString

    function Robot()
        new(randname())
    end
end


function reset!(instance::Robot)
    instance.name = randname()
end

