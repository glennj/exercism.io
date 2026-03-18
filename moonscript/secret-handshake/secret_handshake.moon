import treverse from require './table'

actions = {'wink', 'double blink', 'close your eyes', 'jump'}

{
  commands: (value) ->
    result = [act for i, act in ipairs actions when (value >> (i-1)) & 1 == 1]
    treverse result if (value >> #actions) & 1 == 1
    result
}
