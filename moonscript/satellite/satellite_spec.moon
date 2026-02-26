Satellite = require 'satellite'

describe 'satellite', ->
  it "Empty tree", ->
    result = Satellite.tree {}, {}
    expected = {}
    assert.are.same expected, result

  it "Tree with one item", ->
    result = Satellite.tree {"a"}, {"a"}
    expected = { v: "a", l: {}, r: {} }
    assert.are.same expected, result

  it "Tree with many items", ->
    result = Satellite.tree {"a", "i", "x", "f", "r"}, {"i", "a", "f", "x", "r"}
    expected = {
        v: "a",
        l: { v: "i", l: {}, r: {} },
        r: {
          v: "x",
          l: { v: "f", l: {}, r: {} },
          r: { v: "r", l: {}, r: {} }
        }
      }
    assert.are.same expected, result

  it "Reject traversals of different length", ->
    f = -> Satellite.tree {"a", "b"}, {"b", "a", "r"}
    assert.has.errors f, "traversals must have the same length"

  it "Reject inconsistent traversals of same length", ->
    f = -> Satellite.tree {"x", "y", "z"}, {"a", "b", "c"}
    assert.has.errors f, "traversals must have the same elements"

  it "Reject traversals with repeated items", ->
    f = -> Satellite.tree {"a", "b", "a"}, {"b", "a", "a"}
    assert.has.errors f, "traversals must contain unique items"

  it "A degenerate binary tree", ->
    result = Satellite.tree {"a", "b", "c", "d"}, {"d", "c", "b", "a"}
    expected = {
        v: "a",
        l: {
          v: "b",
          l: {
            v: "c",
            l: { v: "d", l: {}, r: {} },
            r: {}
          },
          r: {}
        },
        r: {}
      }
    assert.are.same expected, result

  it "Another degenerate binary tree", ->
    result = Satellite.tree {"a", "b", "c", "d"}, {"a", "b", "c", "d"}
    expected = {
        v: "a",
        l: {},
        r: {
          v: "b",
          l: {},
          r: {
            v: "c",
            l: {},
            r: { v: "d", l: {}, r: {} }
          }
        }
      }
    assert.are.same expected, result

  it "Tree with many more items", ->
    result = Satellite.tree {"a", "b", "d", "g", "h", "c", "e", "f", "i"}, {"g", "d", "h", "b", "a", "e", "c", "i", "f"}
    expected = {
        v: "a",
        l: {
          v: "b",
          l: {
            v: "d",
            l: { v: "g", l: {}, r: {} },
            r: { v: "h", l: {}, r: {} }
          },
          r: {}
        },
        r: {
          v: "c",
          l: { v: "e", l: {}, r: {} },
          r: {
            v: "f",
            l: { v: "i", l: {}, r: {} },
            r: {}
          }
        }
      }
    assert.are.same expected, result
