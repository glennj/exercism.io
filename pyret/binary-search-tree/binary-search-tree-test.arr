use context starter2024

include file("binary-search-tree.arr")

check "data is retained":
  binary-search-tree([list: 4]) is tree-node(4, empty-tree, empty-tree)
end

check "insert data at proper node -> smaller number at left node":
  expected = tree-node(4,
    tree-node(2, empty-tree, empty-tree),
    empty-tree)

  binary-search-tree([list: 4, 2]) is expected
end

check "insert data at proper node -> same number at left node":
  expected = tree-node(4,
    tree-node(4, empty-tree, empty-tree),
    empty-tree)

  binary-search-tree([list: 4, 4]) is expected
end

check "insert data at proper node -> greater number at right node":
  expected = tree-node(4,
    empty-tree,
    tree-node(5, empty-tree, empty-tree))

  binary-search-tree([list: 4, 5]) is expected
end

check "can create complex tree":
  expected = tree-node(4,
    tree-node(2,
      tree-node(1, empty-tree, empty-tree),
      tree-node(3, empty-tree, empty-tree)),
    tree-node(6,
      tree-node(5, empty-tree, empty-tree),
      tree-node(7, empty-tree, empty-tree)))

  binary-search-tree([list: 4, 2, 6, 1, 3, 5, 7]) is expected
end

check "can sort data -> can sort single number":
  tree = binary-search-tree([list: 2])

  tree.sorted-data() is [list: 2]
end

check "can sort data -> can sort if second number is smaller than first":
  tree = binary-search-tree([list: 2, 1])

  tree.sorted-data() is [list: 1, 2]
end

check "can sort data -> can sort if second number is same as first":
  tree = binary-search-tree([list: 2, 2])

  tree.sorted-data() is [list: 2, 2]
end

check "can sort data -> can sort if second number is greater than first":
  tree = binary-search-tree([list: 2, 3])

  tree.sorted-data() is [list: 2, 3]
end

check "can sort data -> can sort complex tree":
  tree = binary-search-tree([list: 2, 1, 3, 6, 7, 5])

  tree.sorted-data() is [list: 1, 2, 3, 5, 6, 7]
end
