enum Classification { equal, sublist, superlist, unequal }

extension ListEquals<T> on List<T> {
  bool equals<T>(List<T> other) {
    if (this == other) return true;
    if (length != other.length) return false;
    for (var i = 0; i < length; i++) if (this[i] != other[i]) return false;
    return true;
  }

  bool containsSublist<T>(List<T> other) {
    for (var i = 0; i <= length - other.length; i++)
      if (this.sublist(i, i + other.length).equals(other)) return true;
    return false;
  }
}

class Sublist {
  Classification sublist<T>(List<T> one, List<T> two) {
    if (one.equals(two)) return Classification.equal;
    if (one.containsSublist(two)) return Classification.superlist;
    if (two.containsSublist(one)) return Classification.sublist;
    return Classification.unequal;
  }
}
