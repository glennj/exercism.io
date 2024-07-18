import "wren-testie/testie" for Testie, Expect
import "./strain" for Strain

Testie.test("Strain") { |do, skip|
  do.describe("keep") {
    do.test("keep on empty list returns empty list") {
      var actual = Strain.keep([]) { |x| true }
      Expect.value(actual).toEqual([])
    }

    skip.test("keeps everything") {
      var actual = Strain.keep([1, 3, 5]) { |x| true }
      Expect.value(actual).toEqual([1, 3, 5])
    }

    skip.test("keeps nothing") {
      var actual = Strain.keep([1, 3, 5]) { |x| false }
      Expect.value(actual).toEqual([])
    }

    skip.test("keeps first and last") {
      var actual = Strain.keep([1, 2, 3]) { |x| x % 2 == 1 }
      Expect.value(actual).toEqual([1, 3])
    }

    skip.test("keeps neither first nor last") {
      var actual = Strain.keep([1, 2, 3]) { |x| x % 2 == 0 }
      Expect.value(actual).toEqual([2])
    }

    skip.test("keeps strings") {
      var list = ["apple", "zebra", "banana", "zombies", "cherimoya", "zealot"]
      var actual = Strain.keep(list) { |x| x.startsWith("z") }
      var expected = ["zebra", "zombies", "zealot"]
      Expect.value(actual).toEqual(expected)
    }

    skip.test("keeps lists") {
      var list = [
        [1, 2, 3],
        [5, 5, 5],
        [5, 1, 2],
        [2, 1, 2],
        [1, 5, 2],
        [2, 2, 1],
        [1, 2, 5]
      ]
      var actual = Strain.keep(list) { |x| x.contains(5) }
      var expected = [
        [5, 5, 5],
        [5, 1, 2],
        [1, 5, 2],
        [1, 2, 5]
      ]
      Expect.value(actual).toEqual(expected)
    }
  }

  do.describe("discard") {
    skip.test("discard on empty list returns empty list") {
      var actual = Strain.discard([]) { |x| true }
      Expect.value(actual).toEqual([])
    }

    skip.test("discards everything") {
      var actual = Strain.discard([1, 3, 5]) { |x| true }
      Expect.value(actual).toEqual([])
    }

    skip.test("discards nothing") {
      var actual = Strain.discard([1, 3, 5]) { |x| false }
      Expect.value(actual).toEqual([1, 3, 5])
    }

    skip.test("discards first and last") {
      var actual = Strain.discard([1, 2, 3]) { |x| x % 2 == 1 }
      Expect.value(actual).toEqual([2])
    }

    skip.test("discards neither first nor last") {
      var actual = Strain.discard([1, 2, 3]) { |x| x % 2 == 0 }
      Expect.value(actual).toEqual([1, 3])
    }

    skip.test("discards strings") {
      var list = ["apple", "zebra", "banana", "zombies", "cherimoya", "zealot"]
      var actual = Strain.discard(list) { |x| x.startsWith("z") }
      var expected = ["apple", "banana", "cherimoya"]
      Expect.value(actual).toEqual(expected)
    }

    skip.test("discards lists") {
      var list = [
        [1, 2, 3],
        [5, 5, 5],
        [5, 1, 2],
        [2, 1, 2],
        [1, 5, 2],
        [2, 2, 1],
        [1, 2, 5]
      ]
      var actual = Strain.discard(list) { |x| x.contains(5) }
      var expected = [
        [1, 2, 3],
        [2, 1, 2],
        [2, 2, 1]
      ]
      Expect.value(actual).toEqual(expected)
    }
  }
}

