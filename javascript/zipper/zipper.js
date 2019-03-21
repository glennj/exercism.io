/* eslint brace-style: ["error", "stroustrup", { "allowSingleLine": true }] */

const deepCopy = obj => JSON.parse(JSON.stringify(obj));

class Zipper {
  constructor(tree, path) {
    if (path === undefined) {
      this.tree = deepCopy(tree);
      this.path = [];
    }
    else {
      this.tree = tree;
      this.path = path;
    }
  }

  static fromTree(t) {
    const zipper = new Zipper(t);
    return zipper;
  }

  getTree() { return this.tree; }

  toTree() {
    if (this.path[0] !== undefined) {
      return this.path[0].getTree();
    }
    return this.tree;
  }

  value() { return this.tree.value; }

  left() {
    if (this.tree.left !== null) {
      return new Zipper(this.tree.left, this.path.concat(this));
    }
    return null;
  }

  right() {
    if (this.tree.right !== null) {
      return new Zipper(this.tree.right, this.path.concat(this));
    }
    return null;
  }

  up() {
    if (this.path.length > 0) {
      const previous = this.path.pop();
      return new Zipper(previous.getTree(), this.path);
    }
    return null;
  }

  setValue(value) {
    this.tree.value = value;
    return this;
  }

  setLeft(tree) {
    this.tree.left = tree;
    return this;
  }

  setRight(tree) {
    this.tree.right = tree;
    return this;
  }
}

module.exports = Zipper;
