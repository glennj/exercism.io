class Proverb
  @recite: (items) ->
    return '' if items.length is 0
    ("For want of a #{items[i - 1]} the #{items[i]} was lost." for i in [1...items.length])
      .concat ["And all for the want of a #{items[0]}."]
      .join "\n"
      

module.exports = Proverb
