import "./grains" for Grains
import "wren-testie/testie" for Testie, Expect

Testie.test("Grains") { |do, skip|
  do.describe("returns the number of grains on the square") {
    do.test("grains on square 1") {
      Expect.value(Grains.square(1)).toEqual(1)
    }

    do.test("grains on square 2") {
      Expect.value(Grains.square(2)).toEqual(2)
    }

    do.test("grains on square 3") {
      Expect.value(Grains.square(3)).toEqual(4)
    }

    do.test("grains on square 4") {
      Expect.value(Grains.square(4)).toEqual(8)
    }

    do.test("grains on square 16") {
      Expect.value(Grains.square(16)).toEqual(32768)
    }

    do.test("grains on square 32") {
      Expect.value(Grains.square(32)).toEqual(2147483648)
    }

    do.test("grains on square 64") {
      Expect.value(Grains.square(64)).toEqual(9223372036854775808)
    }

    do.test("square 0 raises an exception") {
      Expect.that {
        Grains.square(0)
      }.abortsWith("square must be between 1 and 64")
    }

    do.test("negative square raises an exception") {
      Expect.that {
        Grains.square(-1)
      }.abortsWith("square must be between 1 and 64")
    }

    do.test("square greater than 64 raises an exception") {
      Expect.that {
        Grains.square(65)
      }.abortsWith("square must be between 1 and 64")
    }
  }

  do.test("returns the total number of grains on the board") {
    Expect.value(Grains.total).toEqual(18446744073709551615)
  }
}
