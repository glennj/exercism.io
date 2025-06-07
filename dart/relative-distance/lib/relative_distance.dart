typedef Name = String;
typedef Names = List<Name>;

class RelativeDistance {
  Map<Name, Names> _familyTree;
  RelativeDistance(this._familyTree);

  int degreesOfSeparation(Name a, Name b) {
    var pathA = _pathToRoot(a, []);
    var pathB = _pathToRoot(b, []);

    if (pathA.isEmpty && pathB.isEmpty)
      return -1; // unrelated

    if (pathA.isEmpty)
      return pathB.length;

    if (pathB.isEmpty)
      return pathA.length;

    var paths = _removeCommonPrefix(pathA, pathB);
    return paths.first.length + paths.last.length + 1;
  }
  
  Names _pathToRoot(Name name, Names path) {
    var parent = _findParent(name);
    if (parent == null)
      return path;

    path.insert(0, parent);
    return _pathToRoot(parent, path);
  }

  Name? _findParent(Name child) {
    for (var entry in _familyTree.entries) {
      if (entry.value.contains(child))
        return entry.key;
    }
    return null;
  }

  List<Names> _removeCommonPrefix(Names a, Names b) {
    while (!a.isEmpty && !b.isEmpty && a[0] == b[0]) {
      a.removeAt(0);
      b.removeAt(0);
    }
    return [a, b];
  }
}
