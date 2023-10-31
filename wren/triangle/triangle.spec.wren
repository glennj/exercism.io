import "wren-testie/testie" for Testie, Expect
import "./triangle" for Triangle

Testie.test("Triangle") { |do, skip|
  do.describe("equilateral triangle") {
    do.test("all sides are equal") {
      var triangle = Triangle.new(2, 2, 2)
      Expect.value(triangle.isEquilateral).toBe(true)
    }

    do.test("any side is unequal") {
      var triangle = Triangle.new(2, 3, 2)
      Expect.value(triangle.isEquilateral).toBe(false)
    }

    do.test("no sides are equal") {
      var triangle = Triangle.new(5, 4, 6)
      Expect.value(triangle.isEquilateral).toBe(false)
    }

    do.test("all zero sides is not a triangle") {
      var triangle = Triangle.new(0, 0, 0)
      Expect.value(triangle.isEquilateral).toBe(false)
    }

    do.test("sides may be floats") {
      var triangle = Triangle.new(0.5, 0.5, 0.5)
      Expect.value(triangle.isEquilateral).toBe(true)
    }
  }

  do.describe("isosceles triangle") {
    do.test("last two sides are equal") {
      var triangle = Triangle.new(3, 4, 4)
      Expect.value(triangle.isIsosceles).toBe(true)
    }

    do.test("first two sides are equal") {
      var triangle = Triangle.new(4, 4, 3)
      Expect.value(triangle.isIsosceles).toBe(true)
    }

    do.test("first and last sides are equal") {
      var triangle = Triangle.new(4, 3, 4)
      Expect.value(triangle.isIsosceles).toBe(true)
    }

    do.test("equilateral triangles are also isosceles") {
      var triangle = Triangle.new(4, 4, 4)
      Expect.value(triangle.isIsosceles).toBe(true)
    }

    do.test("no sides are equal") {
      var triangle = Triangle.new(2, 3, 4)
      Expect.value(triangle.isIsosceles).toBe(false)
    }

    do.test("first triangle inequality violation") {
      var triangle = Triangle.new(1, 1, 3)
      Expect.value(triangle.isIsosceles).toBe(false)
    }

    do.test("second triangle inequality violation") {
      var triangle = Triangle.new(1, 3, 1)
      Expect.value(triangle.isIsosceles).toBe(false)
    }

    do.test("third triangle inequality violation") {
      var triangle = Triangle.new(3, 1, 1)
      Expect.value(triangle.isIsosceles).toBe(false)
    }

    do.test("sides may be floats") {
      var triangle = Triangle.new(0.5, 0.4, 0.5)
      Expect.value(triangle.isIsosceles).toBe(true)
    }
  }

  do.describe("scalene triangle") {
    do.test("no sides are equal") {
      var triangle = Triangle.new(5, 4, 6)
      Expect.value(triangle.isScalene).toBe(true)
    }

    do.test("all sides are equal") {
      var triangle = Triangle.new(4, 4, 4)
      Expect.value(triangle.isScalene).toBe(false)
    }

    do.test("two sides are equal") {
      var triangle = Triangle.new(4, 4, 3)
      Expect.value(triangle.isScalene).toBe(false)
    }

    do.test("may not violate triangle inequality") {
      var triangle = Triangle.new(7, 3, 2)
      Expect.value(triangle.isScalene).toBe(false)
    }

    do.test("sides may be floats") {
      var triangle = Triangle.new(0.5, 0.4, 0.6)
      Expect.value(triangle.isScalene).toBe(true)
    }
  }
}
