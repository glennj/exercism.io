class BinarySearchTree<T extends Comparable<T>> {
  T data;
  BinarySearchTree? left;
  BinarySearchTree? right;

  BinarySearchTree(this.data);

  // tests need a "root" property
  BinarySearchTree get root => this;

  void insert(T value) {
    if (value.compareTo(data) <= 0) {
      // using a cascade (`?..`) so the object is returned, not the result of `insert`
      left = (left?..insert(value)) ?? BinarySearchTree<T>(value);
    } else {
      right = (right?..insert(value)) ?? BinarySearchTree<T>(value);
    }
  }

  List<T> get sortedData {
    var values = <T>[];
    this.forEach((T value) => values.add(value));
    return values;
  }

  void forEach<T>(void Function(T) func) {
    left?.forEach(func);
    func(data as T);
    right?.forEach(func);
  }
}
