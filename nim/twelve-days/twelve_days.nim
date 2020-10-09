import algorithm, sequtils, strformat, strutils

const Ordinals = [ "",
  "first", "second", "third", "fourth", "fifth", "sixth",
  "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"
]

const Presents = [ "",
  "a Partridge in a Pear Tree",
  "two Turtle Doves",
  "three French Hens",
  "four Calling Birds",
  "five Gold Rings",
  "six Geese-a-Laying",
  "seven Swans-a-Swimming",
  "eight Maids-a-Milking",
  "nine Ladies Dancing",
  "ten Lords-a-Leaping",
  "eleven Pipers Piping",
  "twelve Drummers Drumming",
]


proc recite*(n: int): string =
  var gifts = Presents[1..n].reversed
  if n > 1:
    gifts[^1] = "and " & gifts[^1]

  &"On the {Ordinals[n]} day of Christmas " &
  &"my true love gave to me: {gifts.join(\", \")}."


proc recite*(m, n: int): string =
  (m..n).toSeq.map(recite).join("\n\n")
