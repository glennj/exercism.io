-- The `re` module ships with the LPeg luarock, which is a dependency of MoonScript.
--    https://luarocks.org/modules/gvvaughan/lpeg
--    https://www.inf.puc-rio.br/~roberto/lpeg/re.html
re = require 're'

-- ------------------------------------------------------------------------
-- This grammar matches a valid question only
wordy = re.compile [[
  wordy      <- 'What is ' {| expression |} '?'
  expression <- { number } (ss { operation } ss { number })*
  number     <- '-'? %d+
  operation  <- 'plus' / 'minus' / 'multiplied by' / 'divided by'
  ss         <- %s+
]]

-- matches syntax errors
syntaxError = re.compile [[
  err       <- no_expr / two_ops / two_nums / ending_op
  no_expr   <- 'What is' s '?'
  two_ops   <- ss op ss op
  two_nums  <- ss num ss num
  ending_op <- ss op '?'
  num       <- '-'? %d+
  op        <- 'plus' / 'minus' / 'multiplied by' / 'divided by'
  s         <- %s*
  ss        <- %s+ 
]]

-- ------------------------------------------------------------------------
evaluate = (tokens) ->
  result = tonumber tokens[1]
  for i = 2, #tokens, 2
    result = switch tokens[i]
      when 'plus'          then result + tonumber tokens[i + 1]
      when 'minus'         then result - tonumber tokens[i + 1]
      when 'multiplied by' then result * tonumber tokens[i + 1]
      when 'divided by'    then result // tonumber tokens[i + 1]
  result

-- ------------------------------------------------------------------------
{
  answer: (question) ->
    tokens = re.match question, wordy
    return evaluate tokens if tokens
    error 'syntax error' if re.find question, syntaxError
    error 'unknown operation'
}
