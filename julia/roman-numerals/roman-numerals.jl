function to_roman(n::Integer)
    n ≥ 1 || error("Cannot render $(n) as Roman")
    roman = ""
    while n ≥ 1000; n -= 1000; roman *=  "M"; end
    if    n ≥  900; n -=  900; roman *= "CM"; end
    if    n ≥  500; n -=  500; roman *=  "D"; end
    if    n ≥  400; n -=  400; roman *= "CD"; end
    while n ≥  100; n -=  100; roman *=  "C"; end
    if    n ≥   90; n -=   90; roman *= "XC"; end
    if    n ≥   50; n -=   50; roman *=  "L"; end
    if    n ≥   40; n -=   40; roman *= "XL"; end
    while n ≥   10; n -=   10; roman *=  "X"; end
    if    n ≥    9; n -=    9; roman *= "IX"; end
    if    n ≥    5; n -=    5; roman *=  "V"; end
    if    n ≥    4; n -=    4; roman *= "IV"; end
    while n ≥    1; n -=    1; roman *=  "I"; end
    roman
end


macro roman_str(s::String)
    to_roman(parse(Int, s))
end
