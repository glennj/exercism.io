class Satellite {
  construct treeFromTraversals(preorder, inorder) {
    if (preorder.count != inorder.count) {
      Fiber.abort("traversals must have the same length")
    }
    if (!preorder.all {|elem| inorder.contains(elem)}) {
      Fiber.abort("traversals must have the same elements")
    }
    if (!listIsUnique(preorder)) {
      Fiber.abort("traversals must contain unique items")
    }

    _tree = null
    _pre = preorder
    _in = inorder
  }

  data {
    if (_tree == null) {
      _tree = buildTree(_pre, _in)
    }
    return _tree
  }

  buildTree(preorder, inorder) {
    if (preorder.count == 0) {
      return null
    }

    var tree = { "data": preorder[0], "left": null, "right": null }
    if (preorder.count > 1) {
      var idx = inorder.indexOf(preorder[0])
      // frustratingly, a range a..b counts backwards if b < a
      var left_preorder = idx == 0 ? [] : preorder[1..idx]
      tree["left"]  = buildTree(left_preorder, inorder[0...idx])
      tree["right"] = buildTree(preorder[idx+1..-1], inorder[idx+1..-1])
    }
    return tree
  }

  listIsUnique(list) {
    var seen = {}
    for (elem in list) {
      if (seen.containsKey(elem)) {
        return false
      }
      seen[elem] = true
    }
    return true
  }
}
