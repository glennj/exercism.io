export default class BinarySearchTree {
  constructor(data) {
    this.data = data;
    this.left = null;
    this.right = null;
  }

  insert(data) {
    if (data <= this.data) {
      if (this.left) {
        this.left.insert(data);
      } else {
        this.left = new BinarySearchTree(data);
      }
    } else if (data > this.data) {
      if (this.right) {
        this.right.insert(data);
      } else {
        this.right = new BinarySearchTree(data);
      }
    }
  }

  // walk the tree
  each(callback) {
    if (this.left) this.left.each(callback);
    callback(this.data);
    if (this.right) this.right.each(callback);
  }
}
