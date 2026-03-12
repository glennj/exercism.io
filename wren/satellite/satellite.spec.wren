import "./satellite" for Satellite
import "wren-testie/testie" for Testie, Expect

Testie.test("Satellite") { |do, skip|
  do.test("Empty tree") {
    var preorder = []
    var inorder = []
    var tree = Satellite.treeFromTraversals(preorder, inorder)
    Expect.value(tree.data).toEqual(null)
  }

  do.test("Tree with one item") {
    var preorder = ["a"]
    var inorder = ["a"]
    var tree = Satellite.treeFromTraversals(preorder, inorder)
    Expect.value(tree.data).toEqual({
      "data": "a",
      "left": null,
      "right": null
    })
  }

  do.test("Tree with many items") {
    var preorder = ["a", "i", "x", "f", "r"]
    var inorder = ["i", "a", "f", "x", "r"]
    var tree = Satellite.treeFromTraversals(preorder, inorder)
    Expect.value(tree.data).toEqual({
      "data": "a",
      "left": {
        "data": "i",
        "left": null,
        "right": null
      },
      "right": {
        "data": "x",
        "left": {
          "data": "f",
          "left": null,
          "right": null
        },
        "right": {
          "data": "r",
          "left": null,
          "right": null
        }
      }
    })
  }

  do.test("Reject traversals of different length") {
    var preorder = ["a", "b"]
    var inorder = ["b", "a", "r"]
    Expect.that {
      Satellite.treeFromTraversals(preorder, inorder)
    }.abortsWith("traversals must have the same length")
  }

  do.test("Reject inconsistent traversals of same length") {
    var preorder = ["x", "y", "z"]
    var inorder = ["a", "b", "c"]
    Expect.that {
      Satellite.treeFromTraversals(preorder, inorder)
    }.abortsWith("traversals must have the same elements")
  }

  do.test("Reject traversals with repeated items") {
    var preorder = ["a", "b", "a"]
    var inorder = ["b", "a", "a"]
    Expect.that {
      Satellite.treeFromTraversals(preorder, inorder)
    }.abortsWith("traversals must contain unique items")
  }

  do.test("A degenerate binary tree") {
    var preorder = ["a", "b", "c", "d"]
    var inorder = ["d", "c", "b", "a"]
    var tree = Satellite.treeFromTraversals(preorder, inorder)
    Expect.value(tree.data).toEqual({
      "data": "a",
      "left": {
        "data": "b",
        "left": {
          "data": "c",
          "left": {
            "data": "d",
            "left": null,
            "right": null
          },
          "right": null
        },
        "right": null
      },
      "right": null
    })
  }

  do.test("Another degenerate binary tree") {
    var preorder = ["a", "b", "c", "d"]
    var inorder = ["a", "b", "c", "d"]
    var tree = Satellite.treeFromTraversals(preorder, inorder)
    Expect.value(tree.data).toEqual({
      "data": "a",
      "left": null,
      "right": {
        "data": "b",
        "left": null,
        "right": {
          "data": "c",
          "left": null,
          "right": {
            "data": "d",
            "left": null,
            "right": null
          }
        }
      }
    })
  }

  do.test("Tree with many more items") {
    var preorder = ["a", "b", "d", "g", "h", "c", "e", "f", "i"]
    var inorder = ["g", "d", "h", "b", "a", "e", "c", "i", "f"]
    var tree = Satellite.treeFromTraversals(preorder, inorder)
    Expect.value(tree.data).toEqual({
      "data": "a",
      "left": {
        "data": "b",
        "left": {
          "data": "d",
          "left": {
            "data": "g",
            "left": null,
            "right": null
          },
          "right": {
            "data": "h",
            "left": null,
            "right": null
          }
        },
        "right": null
      },
      "right": {
        "data": "c",
        "left": {
          "data": "e",
          "left": null,
          "right": null
        },
        "right": {
          "data": "f",
          "left": {
            "data": "i",
            "left": null,
            "right": null
          },
          "right": null
        }
      }
    })
  }
}
