Nth = [
  'first', 'second', 'third', 'fourth', 'fifth', 'sixth',
  'seventh', 'eighth', 'ninth', 'tenth', 'eleventh', 'twelfth',
]

Gifts = [
  'a Partridge in a Pear Tree.',
  'two Turtle Doves,',
  'three French Hens,',
  'four Calling Birds,',
  'five Gold Rings,',
  'six Geese-a-Laying,',
  'seven Swans-a-Swimming,',
  'eight Maids-a-Milking,',
  'nine Ladies Dancing,',
  'ten Lords-a-Leaping,',
  'eleven Pipers Piping,',
  'twelve Drummers Drumming,',
]

class TwelveDays
  @recite: (startVerse, endVerse) ->
    (@day i for i in [startVerse .. endVerse]).join("\n")

  @day: (n) ->
    list = Gifts[0...n].reverse()
    list.splice 0, 0, "On the #{Nth[n - 1]} day of Christmas my true love gave to me:"
    list.splice -1, 0, 'and' if n > 1
    list.join ' '

module.exports = TwelveDays
