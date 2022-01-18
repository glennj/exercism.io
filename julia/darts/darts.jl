function score(x, y)
    dist = hypot(x, y)
    if dist ≤ 1
        10
    elseif dist ≤ 5
        5
    elseif dist ≤ 10
        1
    else
        0
    end
end
