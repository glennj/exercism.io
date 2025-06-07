typedef Name = String;
typedef Names = List<Name>;

class RelativeDistance {
  Map<Name, Names> _familyTree;
  RelativeDistance(this._familyTree);

  int degreesOfSeparation(Name a, Name b) {
    var pathA = _pathToRoot(a, []);
    var pathB = _pathToRoot(b, []);

    // unrelated cases
    if (pathA.isEmpty && pathB.isEmpty) return -1;
    if (pathA.isEmpty) return (a != pathB[0]) ? -1 : pathB.length;
    if (pathB.isEmpty) return (b != pathA[0]) ? -1 : pathA.length;
    if (pathA[0] != pathB[0]) return -1;

    // remove common prefix
    while (!pathA.isEmpty && !pathB.isEmpty && pathA[0] == pathB[0]) {
      pathA.removeAt(0);
      pathB.removeAt(0);
    }
    return pathA.length + pathB.length + 1;
  }

  Names _pathToRoot(Name name, Names path) {
    var parent = _findParent(name);
    if (parent == null) return path;

    path.insert(0, parent);
    return _pathToRoot(parent, path);
  }

  Name? _findParent(Name child) {
    for (var entry in _familyTree.entries) {
      if (entry.value.contains(child)) return entry.key;
    }
    return null;
  }
}
