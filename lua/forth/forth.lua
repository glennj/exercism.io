local List = require('./list')
local ForthStack = require('./forth_stack')

return {
  evaluate = function(instructions)
    local stack = ForthStack:new()
    local words = List:new()
    
    for word in table.concat(instructions, ' '):lower():gmatch('%S+') do
      words:push(word)
    end
  
    while #words > 0 do
      local word = words:shift()
  
      if stack:has_user_word(word) then words:prepend_all(stack:get_user_word(word))
      elseif word == ":"    then stack:define_word(words)
      elseif word == "drop" then stack:drop()
      elseif word == "dup"  then stack:dup()
      elseif word == "swap" then stack:swap()
      elseif word == "over" then stack:over()
      elseif word == "+"    then stack:add()
      elseif word == "-"    then stack:sub()
      elseif word == "*"    then stack:mul()
      elseif word == "/"    then stack:div()
      else
        local n = tonumber(word)
        if n then stack:push(n) else error('unknown') end
      end
    end

    return stack:toTable()
  end
}
