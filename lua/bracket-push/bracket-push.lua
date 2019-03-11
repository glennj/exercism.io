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

--[[ 
Dammit, this is what I *wanted* to do

https://exercism.io/tracks/lua/exercises/bracket-push/solutions/0eb2073ce38643ccbe087f94da0dcc83 

        local pairs = {
            [')'] = '(',
            [']'] = '[',
            ['}'] = '{'
        }

        local function valid(input)
            local stack = {}

            for c in input:gmatch('[()[%]{}]') do
                if c:match('[([{]') then
                    table.insert(stack, c)
                elseif table.remove(stack) ~= pairs[c] then
                    return false
                end
            end

            return next(stack) == nil
        end
        return { valid = valid }
    
--]]
