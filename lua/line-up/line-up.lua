local ordinal = function(number)
    local ones = number % 10
    local tens = number % 100
    if ones == 1 and tens ~= 11 then return "st" end
    if ones == 2 and tens ~= 12 then return "nd" end
    if ones == 3 and tens ~= 13 then return "rd" end
    return "th"
end

return {
  format = function(name, number)
      return name .. ", you are the " .. number .. ordinal(number) ..
             " customer we serve today. Thank you!"
  end
}
