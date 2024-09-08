class MatchingBrackets
  @isPaired: (value) ->
    stack = []
    
    for char in value
      switch char
        when '(', '[', '{' 
          stack.push char
        when ')', ']', '}' 
          return false if stack.length is 0
          return false unless stack.pop() + char in [ '()', '[]', '{}' ]

    return stack.length is 0

module.exports = MatchingBrackets
