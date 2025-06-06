const HouseData = [
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
];

class House {
  String recite(int first, int last) =>
      List.generate(last - first + 1, (i) => first + i)
          .map(_verse)
          .join('\n');

  String _verse(int start) =>
      List.generate(start, (i) => start - 1 - i)
          .fold(
              StringBuffer("This is"),
              (verse, i) => verse
                  ..write(" the ")..write(HouseData[i].first)
                  ..write(" that ")..write(HouseData[i].last))
          .toString();
}
