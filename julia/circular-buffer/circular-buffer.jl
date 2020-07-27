"""
Implemented with a plain array.
The "read header" is always at the beginning of the array.
The "write header" is always at the end of the array.

The push!, popfirst!, and empty! functions are pretty thin
wrappers around the array's respective functions.
"""

mutable struct CircularBuffer{T}
    data::Array{T,1}
    capacity::Integer

    function CircularBuffer{T}(capacity::Integer) where {T}
        new(Array{T,1}(), capacity)
    end
end

isfull(cb::CircularBuffer) = length(cb.data) == cb.capacity

function Base.push!(cb::CircularBuffer, item; overwrite::Bool=false)
    if isfull(cb)
        if overwrite
            popfirst!(cb)
        else
            throw(BoundsError())
        end
    end
    push!(cb.data, item)
    cb
end

function Base.popfirst!(cb::CircularBuffer)
    if isempty(cb.data)
        throw(BoundsError())
    end
    popfirst!(cb.data)
end

function Base.empty!(cb::CircularBuffer)
    empty!(cb.data)
    cb
end
