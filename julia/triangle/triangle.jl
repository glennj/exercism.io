module Triangle
    export is_equilateral, is_isosceles, is_scalene

    @enum Classification begin
        invalid
        equilateral
        isosceles
        scalene
    end

    function is_valid(sides)
        valid = false
        if length(sides) == 3
            a, b, c = sort(sides)
            valid = a > 0 && a + b > c
        end
        valid
    end

    function classification(sides)
        cls = invalid
        if is_valid(sides)
            set = Set(sides)
            len = length(set)

            if len == 1
                cls = equilateral
            elseif len == 2
                cls = isosceles
            else
                cls = scalene
            end
        end
        cls
    end

    function is_equilateral(sides)
        classification(sides) == equilateral
    end

    function is_isosceles(sides)
        in(classification(sides), [equilateral, isosceles])
    end

    function is_scalene(sides)
        classification(sides) == scalene
    end
end

using .Triangle
