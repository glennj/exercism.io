import "wren-testie/testie" for Testie, Expect
import "./binary-search-tree" for BinarySearchTree

Testie.test("Binary Search Tree") { |do, skip|
  do.test("data is retained") {
    var tree = BinarySearchTree.with(4)
    Expect.value(tree.data).toEqual({
      "data": 4,
      "left": null,
      "right": null
    })
  }

  do.describe("insert data at proper node") {
    skip.test("smaller number at left node") {
      var tree = BinarySearchTree.withAll([4, 2])
      Expect.value(tree.data).toEqual({
        "data": 4,
        "left": {
          "data": 2,
          "left": null,
          "right": null
        },
        "right": null
      })
    }

    skip.test("same number at left node") {
      var tree = BinarySearchTree.withAll([4, 4])
      Expect.value(tree.data).toEqual({
        "data": 4,
        "left": {
          "data": 4,
          "left": null,
          "right": null
        },
        "right": null
      })
    }

    skip.test("greater number at right node") {
      var tree = BinarySearchTree.withAll([4, 5])
      Expect.value(tree.data).toEqual({
        "data": 4,
        "left": null,
        "right": {
          "data": 5,
          "left": null,
          "right": null
        }
      })
    }

    skip.test("can create complex tree") {
      var tree = BinarySearchTree.withAll([4, 2, 6, 1, 3, 5, 7])
      Expect.value(tree.data).toEqual({
        "data": 4,
        "left": {
          "data": 2,
          "left": {
            "data": 1,
            "left": null,
            "right": null
          },
          "right": {
            "data": 3,
            "left": null,
            "right": null
          }
        },
        "right": {
          "data": 6,
          "left": {
            "data": 5,
            "left": null,
            "right": null
          },
          "right": {
            "data": 7,
            "left": null,
            "right": null
          }
        }
      })
    }
  }

  do.describe("can sort data") {
    skip.test("can sort single number") {
      var tree = BinarySearchTree.withAll([2])
      Expect.value(tree.sortedData).toEqual([2])
    }

    skip.test("can sort if second number is smaller than first") {
      var tree = BinarySearchTree.withAll([2, 1])
      Expect.value(tree.sortedData).toEqual([1, 2])
    }

    skip.test("can sort if second number is same as first") {
      var tree = BinarySearchTree.withAll([2, 2])
      Expect.value(tree.sortedData).toEqual([2, 2])
    }

    skip.test("can sort if second number is greater than first") {
      var tree = BinarySearchTree.withAll([2, 3])
      Expect.value(tree.sortedData).toEqual([2, 3])
    }

    skip.test("can sort complex tree") {
      var tree = BinarySearchTree.withAll([2, 1, 3, 6, 7, 5])
      Expect.value(tree.sortedData).toEqual([1, 2, 3, 5, 6, 7])
    }
  }
}
