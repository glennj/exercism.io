ordinals = {
  'first', 'second', 'third', 'fourth', 'fifth', 'sixth',
  'seventh', 'eighth', 'ninth', 'tenth', 'eleventh', 'twelfth',
}

gifts = {
  'a Partridge in a Pear Tree',
  'two Turtle Doves',
  'three French Hens',
  'four Calling Birds',
  'five Gold Rings',
  'six Geese-a-Laying',
  'seven Swans-a-Swimming',
  'eight Maids-a-Milking',
  'nine Ladies Dancing',
  'ten Lords-a-Leaping',
  'eleven Pipers Piping',
  'twelve Drummers Drumming',
}

gift = (i, day) ->
  if i == 1 and day > 1 then "and #{gifts[1]}" else gifts[i]

fmt = 'On the %s day of Christmas my true love gave to me: %s.'

verse = (day) ->
  gs = [gift i, day for i = day, 1, -1]
  string.format fmt, ordinals[day], table.concat(gs, ', ')


{
  recite: (start, stop) ->
    [verse day for day = start, stop]
}
