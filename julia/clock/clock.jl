using Printf
using Dates
import Base: +, -, ==, show


struct Clock
    hours::Int
    minutes::Int

    function Clock(hours::Int, minutes::Int)
        d_hrs, mins = divrem(minutes, 60)
        while mins < 0
            mins += 60
            d_hrs -= 1
        end

        hrs = (hours + d_hrs) % 24
        while hrs < 0
            hrs += 24
        end

        new(hrs, mins)
    end

end


# add/subtract minutes
function +(clock::Clock, minutes::Minute)::Clock
    Clock(clock.hours, clock.minutes + minutes.value)
end

function -(clock::Clock, minutes::Minute)::Clock
    clock + -minutes
end


# equality
function ==(a::Clock, b::Clock)
    a.hours == b.hours && a.minutes == b.minutes
end


function show(io::IO, c::Clock)
    timestamp = @sprintf("%02d:%02d", c.hours, c.minutes)
    show(io, timestamp)
end
