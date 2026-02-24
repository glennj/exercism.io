line = (a, b) -> "For want of a #{a} the #{b} was lost.\n"

return (items) ->
  proverb = {}
  if #items > 0
    proverb = [line items[i - 1], items[i] for i = 2, #items]
    table.insert proverb, "And all for the want of a #{items[1]}.\n"
  table.concat proverb
