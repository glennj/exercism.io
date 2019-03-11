local raindrops = function(n)
    local result = ""
    if n % 3 == 0   then result = result .. "Pling" end
    if n % 5 == 0   then result = result .. "Plang" end
    if n % 7 == 0   then result = result .. "Plong" end
    if result == "" then result = tostring(n) end
    return result
end

return raindrops
