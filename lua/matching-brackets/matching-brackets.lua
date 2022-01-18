local function is_balanced(input)
    local stack = {} 
    for char in input:gmatch(".") do
        if char == "{" or char == "[" or char == "(" then
            table.insert(stack, char)
        elseif char == "}" and table.remove(stack) ~= "{" then
            return false
        elseif char == "]" and table.remove(stack) ~= "[" then
            return false
        elseif char == ")" and table.remove(stack) ~= "(" then
            return false
        end
    end
    return #stack == 0
end

return { valid = is_balanced }
