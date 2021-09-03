import "./accumulate" for Accumulate
import "wren-testie/testie" for Testie, Expect

Testie.test("accumulate") { |do, skip|
  do.test("accumulation empty") {
    Expect.value(Accumulate.call([]) { |e| e * e }).toEqual([])
  }

  skip.test("accumulate squares") {
    var result = Accumulate.call([1, 2, 3]) { |e| e * e }
    Expect.value(result).toEqual([1, 4, 9])
  }

  skip.test("accumulate hellos") {
    var fn = Fn.new { |name| "hello, %(name)" }
    var result = Accumulate.call("bob mary sue".split(" "), fn)
    Expect.value(result).toEqual(["hello, bob", "hello, mary", "hello, sue"])
  }

  skip.test("accumulate reversed strings") {
    var result = Accumulate.call("the quick brown fox etc".split(" ")) { |word| word[-1..0] }
    Expect.value(result).toEqual(["eht", "kciuq", "nworb", "xof", "cte"])
  }

  skip.test("accumulate recursively") {
    var result = Accumulate.call("a b c".split(" ")) { |char |
      return Accumulate.call("1 2 3".split(" ")) { |digit |
        return char + digit
      }
    }

    Expect.value(result).toEqual([
      ["a1", "a2", "a3"],
      ["b1", "b2", "b3"],
      ["c1", "c2", "c3"],
    ])
  }
}
