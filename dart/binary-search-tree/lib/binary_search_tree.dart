class BinarySearchTree<T extends Comparable<T>> {
  T data;
  BinarySearchTree? left;
  BinarySearchTree? right;

  BinarySearchTree(this.data);

  // tests need a "root" property
  BinarySearchTree get root => this;

  void insert(T value) {
    if (value.compareTo(this.data) <= 0) {
      if (this.left == null) {
        this.left = BinarySearchTree<T>(value);
      } else {
        this.left!.insert(value);
      }
    } else {
      if (this.right == null) {
        this.right = BinarySearchTree<T>(value);
      } else {
        this.right!.insert(value);
      }
    }
  }

  List<T> get sortedData {
    var values = <T>[];
    this.forEach((T value) => values.add(value));
    return values;
  }

  void forEach<T>(void Function(T) func) {
    this.left?.forEach(func);
    func(this.data as T);
    this.right?.forEach(func);
  }
}
