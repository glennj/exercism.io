class Flatten {
  static flatten(seq) {
    return seq
      .reduce([]) {|flattened, elem|
        if (elem is Sequence) {
          flattened.addAll(flatten(elem))
        } else {
          flattened.add(elem)
        }
        return flattened
      }
      .where {|elem| elem != null}
      .toList
  }
}
