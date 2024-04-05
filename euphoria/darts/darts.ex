public function score(atom x, atom y)
    atom dist = sqrt(x * x + y * y)
    if dist <= 1 then
        return 10
    elsif dist <= 5 then
        return 5
    elsif dist <= 10 then
        return 1
    else
        return 0
    end if
end function
