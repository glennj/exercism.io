const ordinal = [
  "No", "One", "Two", "Three", "Four", "Five",
  "Six", "Seven", "Eight", "Nine", "Ten"
];

class BottleSong {
  List<String> recite(int start, int count) =>
      List.generate(count, (i) => start - i)
          .map(_verse)
          .fold(<String>[], (all, lines) => all..add('')..addAll(lines)) // cascade notation
          .skip(1)
          .toList();

  List<String> _verse(int n) {
    var lines = ["${_bottle(n)} hanging on the wall,"];
    lines.add(lines.last);
    lines.add("And if one green bottle should accidentally fall,");
    lines.add("There'll be ${_bottle(n - 1).toLowerCase()} hanging on the wall.");
    return lines;
  }

  String _bottle(int n) => "${ordinal[n]} green bottle${n == 1 ? "" : "s"}";
}
