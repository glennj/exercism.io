// Package binarysearchtree comment ...
package binarysearchtree

// BinarySearchTree type comment ...
type BinarySearchTree struct {
	left  *BinarySearchTree
	data  int
	right *BinarySearchTree
}

// NewBst creates and returns a new BinarySearchTree.
func NewBst(i int) *BinarySearchTree {
	return &BinarySearchTree{data: i}
}

// Insert inserts an int into the BinarySearchTree.
// Inserts happen based on the rules of a binary search tree
func (bst *BinarySearchTree) Insert(i int) {
	if i <= bst.data {
		if bst.left == nil {
			bst.left = NewBst(i)
		} else {
			bst.left.Insert(i)
		}
	} else {
		if bst.right == nil {
			bst.right = NewBst(i)
		} else {
			bst.right.Insert(i)
		}
	}
}

// ForEach walks the tree in order, executing a function on each node's data
func (bst *BinarySearchTree) ForEach(fn func(int)) {
	if bst.left != nil {
		bst.left.ForEach(fn)
	}
	fn(bst.data)
	if bst.right != nil {
		bst.right.ForEach(fn)
	}
}

// SortedData returns the ordered contents of BinarySearchTree as an []int.
// The values are in increasing order starting with the lowest int value.
// A BinarySearchTree that has the numbers [1,3,7,5] added will return the
// []int [1,3,5,7].
func (bst *BinarySearchTree) SortedData() (data []int) {
	bst.ForEach(func(value int) { data = append(data, value) })
	return
}

// bench
// BenchmarkSortedData      3417530               452.5 ns/op           248 B/op          5 allocs/op
