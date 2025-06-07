HouseData = [
  ["house", "Jack built."],
  ["malt", "lay in"],
  ["rat", "ate"],
  ["cat", "killed"],
  ["dog", "worried"],
  ["cow with the crumpled horn", "tossed"],
  ["maiden all forlorn", "milked"],
  ["man all tattered and torn", "kissed"],
  ["priest all shaven and shorn", "married"],
  ["rooster", "crowed in the morn that woke"],
  ["farmer sowing his corn", "kept"],
  ["horse and the hound and the horn", "belonged to"],
]

class House
  @verse: (number) ->
    [number - 1 .. 0].reduce(
      (verse, i) -> "#{verse} the #{HouseData[i][0]} that #{HouseData[i][1]}",
      "This is"
    )

  @verses: (startVerse, endVerse) ->
    (@verse i for i in [startVerse .. endVerse]).join("\n")

module.exports = House