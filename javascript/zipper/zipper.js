const deepCopy = obj => JSON.parse(JSON.stringify(obj));

export class Zipper {
  constructor(tree, path = []) {
    this.tree = tree;
    this.path = path;
  }

  static fromTree(tree) { return new Zipper(deepCopy(tree)); }

  getTree() { return this.tree; }

  toTree() {
    return this.path.length > 0
      ? this.path[0].getTree()
      : this.tree;
  }

  value() { return this.tree.value; }

  left() {
    return this.tree.left
      ? new Zipper(this.tree.left, this.path.concat(this))
      : null;
  }

  right() {
    return this.tree.right
      ? new Zipper(this.tree.right, this.path.concat(this))
      : null;
  }

  up() {
    return this.path.length > 0
      ? new Zipper(this.path.pop().getTree(), this.path)
      : null;
  }

  setValue(value) {
    this.tree.value = value;
    return new Zipper(this.toTree());
  }

  setLeft(tree) {
    this.tree.left = tree;
    return new Zipper(this.toTree());
  }

  setRight(tree) {
    this.tree.right = tree;
    return new Zipper(this.toTree());
  }
}
