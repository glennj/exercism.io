function score(x, y)
	return dist > 10 ? 0 : (dist > 5 ? 1 : ( dist > 1 ? 5 : 10 ))
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
