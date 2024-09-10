class BottleSong
  numbers = ['No', 'One', 'Two', 'Three', 'Four', 'Five', 'Six', 'Seven', 'Eight', 'Nine', 'Ten']
  bottles = (n, tolower = false, wall = true) -> 
    b = "#{numbers[n]} green bottle"
    b += "s" unless n == 1
    b += " hanging on the wall" if wall
    if tolower then b.toLowerCase() else b
  
  recite: (start, count) ->
    verses = []
    for i in [0 ... count]
      verses.push "", @verse(start - i)...
    verses.slice(1)

  verse: (n) -> [
      "#{bottles n, false, true},"
      "#{bottles n, false, true},"
      "And if #{bottles 1, true, false} should accidentally fall,"
      "There'll be #{bottles n - 1, true, true}."
    ]
    
module.exports = BottleSong
