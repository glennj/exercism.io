class BinarySearchTree {
  construct new() {
    _item = null
    _left = null
    _right = null
  }

  construct with(item)     { add(item) }
  construct withAll(items) { items.each {|item| add(item)} }

  item { _item }

  data { 
    return {
      "data":  _item,
      "left":  _left == null  ? null : _left.data,
      "right": _right == null ? null : _right.data,
    }
  }

  add(item) {
    if (_item == null) {
      _item = item
    } else if (item <= _item) {
      if (_left == null) {
        _left = this.type.with(item)
      } else {
        _left.add(item)
      }
    } else {
      if (_right == null) {
        _right = this.type.with(item)
      } else {
        _right.add(item)
      }
    }
  }

  sortedData {
    var items = []
    for (node in inorderWalk()) {
      items.add(node.item)
    }
    return items
  }

  inorderWalk() {
    var nodes = []
    if (_left != null)  nodes.addAll(_left.inorderWalk())
    nodes.add(this)
    if (_right != null) nodes.addAll(_right.inorderWalk())
    return nodes
  }
}
