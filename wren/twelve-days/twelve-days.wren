var Ordinals = ["", 
  "first", "second", "third", "fourth", "fifth", "sixth",
  "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"
]

var Gifts = ["",
  "a Partridge in a Pear Tree", "two Turtle Doves",
  "three French Hens", "four Calling Birds",
  "five Gold Rings", "six Geese-a-Laying",
  "seven Swans-a-Swimming", "eight Maids-a-Milking",
  "nine Ladies Dancing", "ten Lords-a-Leaping",
  "eleven Pipers Piping", "twelve Drummers Drumming",
]

class TwelveDays {
  static recite(n) {
    var gifts = Gifts[n..1] // reverse Ranges, cool
    if (n > 1) gifts[-1] = "and " + gifts[-1]
    return "On the %(Ordinals[n]) day of Christmas" +
      " my true love gave to me: %(gifts.join(", ")).\n"
  }

  static recite(i, j) { (i..j).map {|n| recite(n)}.join("\n") }
}
