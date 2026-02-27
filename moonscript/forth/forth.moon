ForthStack = require 'forth_stack'
List       = require 'list'

class Forth
  new: =>
    @stk = ForthStack!
    @words = {}

  stack: =>
    @stk\values!

  evaluate: (script) =>
    all = table.concat script, ' '
    @tokens = List [token for token in all\lower!\gmatch '%S+']

    while not @tokens\is_empty!
      token = @tokens\shift!

      if number = tonumber token then @stk\push number
      elseif @words[token]       then @tokens\unshift_all @words[token]
      else
        switch token
          when ':'                           then @record_word!
          when '+', '-', '*', '/'            then @stk\arithmetic token
          when 'dup', 'drop', 'swap', 'over' then @stk\builtin token
          else error 'undefined operation'

  record_word: =>
    name = @tokens\shift!
    assert not tonumber(name), 'illegal operation'
    return if name == ';'   -- empty definition

    definition = {}
    while true
      token = @tokens\shift!
      break if token == ';'
      if @words[token]
        table.insert definition, t for t in *@words[token]
      else
        table.insert definition, token
    @words[name] = definition
