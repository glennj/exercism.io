local Queen = function(position)
    assert(
        position
        and 0 <= position.row    and position.row    <= 7
        and 0 <= position.column and position.column <= 7
        , 'invalid position'
    ) 
    return {
        position = position,
        can_attack = function (other)
            local dx = math.abs(position.row - other.position.row)
            local dy = math.abs(position.column - other.position.column)
            return dx == 0 or dy == 0 or dx == dy
        end
    }
end

return Queen
