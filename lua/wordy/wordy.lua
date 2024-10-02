return {
  answer = function(question)
    assert(question:match('^What is'), 'Invalid question')

    local stack = {}
    for word in question:gsub('What is%s*(.*)?', '%1'):gmatch('%S+') do
      stack[#stack+1] = word
    end
    assert(#stack > 0, 'Invalid question')

    -- -------------------------------------
    local shift = function()
      local value = stack[1]
      stack = { table.unpack(stack, 2) }
      return value
    end

    local get_number = function()
      assert(#stack > 0, 'Invalid question')
      local number = tonumber(shift())
      assert(number, 'Invalid question')
      return number
    end

    local get_operation = function()
      local op
      op = shift()
      if op == 'plus' or op == 'minus' then
        return op
      elseif op == 'multiplied' or op == 'divided' then
        local word = shift()
        assert(word == 'by', 'Invalid question')
        return op
      else
        error('Invalid question')
      end
    end

    local perform_operation = function(a, op, b)
      if     op == 'plus'       then return a + b
      elseif op == 'minus'      then return a - b
      elseif op == 'multiplied' then return a * b
      elseif op == 'divided'    then return a / b -- TODO test div by zero
      end
    end
    -- -------------------------------------

    -- evaluate the expression
    local result = get_number()
    while #stack > 0 do
      local op = get_operation()
      local n = get_number()
      result = perform_operation(result, op, n)
    end
    return result
  end
}
