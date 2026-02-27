List = require 'list'

class ForthStack extends List
  arithmetic: (op) =>
    @need 2
    b = @pop!
    a = @pop!
    switch op
      when '+' then @push(a + b)
      when '-' then @push(a - b)
      when '*' then @push(a * b)
      when '/'
        assert b != 0, 'divide by zero'
        @push(a // b)

  builtin: (cmd) => -- dup, drop, swap, over
    self[cmd] self

  dup: =>
    @need 1
    @push @peek!

  drop: =>
    @need 1
    @pop!
    return

  swap: =>
    @need 2
    b = @pop!
    a = @pop!
    @push b
    @push a

  over: =>
    @need 2
    b = @pop!
    a = @peek!
    @push b
    @push a


  need: (n) =>
    if #@data == 0 then error 'empty stack'
    if #@data < n  then error 'only one value on the stack'
