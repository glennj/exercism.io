var Items = [
  null,
  "the house that Jack built",
  "the malt that lay in",
  "the rat that ate",
  "the cat that killed",
  "the dog that worried",
  "the cow with the crumpled horn that tossed",
  "the maiden all forlorn that milked",
  "the man all tattered and torn that kissed",
  "the priest all shaven and shorn that married",
  "the rooster that crowed in the morn that woke",
  "the farmer sowing his corn that kept",
  "the horse and the hound and the horn that belonged to",
]

class House {
  static recite(start, end) { (start .. end).map {|i| verse(i)}.toList }

  static verse(n) { "This is " + Items[n..1].join(" ") + "." }
}
