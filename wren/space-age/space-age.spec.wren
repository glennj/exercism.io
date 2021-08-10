import "wren-testie/testie" for Testie, Expect
import "./space-age" for Space

Testie.test("Space Age") { |do, skip|
  do.test("age on Earth") {
    Expect.value(Space.age("earth", 1000000000)).toEqual(31.69)
  }

  do.test("age on Mercury") {
    Expect.value(Space.age("mercury", 2134835688)).toEqual(280.88)
  }

  do.test("age on Venus") {
    Expect.value(Space.age("venus", 189839836)).toEqual(9.78)
  }

  do.test("age on Mars") {
    Expect.value(Space.age("mars", 2129871239)).toEqual(35.88)
  }

  do.test("age on Jupiter") {
    Expect.value(Space.age("jupiter", 901876382)).toEqual(2.41)
  }

  do.test("age on Saturn") {
    Expect.value(Space.age("saturn", 2000000000)).toEqual(2.15)
  }

  do.test("age on Uranus") {
    Expect.value(Space.age("uranus", 1210123456)).toEqual(0.46)
  }

  do.test("age on Neptune") {
    Expect.value(Space.age("neptune", 1821023456)).toEqual(0.35)
  }
}
