import "./raindrops" for Raindrops
import "wren-testie/testie" for Testie, Expect

Testie.test("Raindrops") { |do, skip|
  do.test("the sound for 1 is 1") {
    Expect.value(Raindrops.convert(1)).toEqual("1")
  }

  skip.test("the sound for 3 is Pling") {
    Expect.value(Raindrops.convert(3)).toEqual("Pling")
  }

  skip.test("the sound for 5 is Plang") {
    Expect.value(Raindrops.convert(5)).toEqual("Plang")
  }

  skip.test("the sound for 7 is Plong") {
    Expect.value(Raindrops.convert(7)).toEqual("Plong")
  }

  skip.test("the sound for 6 is Pling as it has a factor 3") {
    Expect.value(Raindrops.convert(6)).toEqual("Pling")
  }

  skip.test("2 to the power 3 does not make a raindrop sound as 3 is the exponent not the base") {
    Expect.value(Raindrops.convert(8)).toEqual("8")
  }

  skip.test("the sound for 9 is Pling as it has a factor 3") {
    Expect.value(Raindrops.convert(9)).toEqual("Pling")
  }

  skip.test("the sound for 10 is Plang as it has a factor 5") {
    Expect.value(Raindrops.convert(10)).toEqual("Plang")
  }

  skip.test("the sound for 14 is Plong as it has a factor of 7") {
    Expect.value(Raindrops.convert(14)).toEqual("Plong")
  }

  skip.test("the sound for 15 is PlingPlang as it has factors 3 and 5") {
    Expect.value(Raindrops.convert(15)).toEqual("PlingPlang")
  }

  skip.test("the sound for 21 is PlingPlong as it has factors 3 and 7") {
    Expect.value(Raindrops.convert(21)).toEqual("PlingPlong")
  }

  skip.test("the sound for 25 is Plang as it has a factor 5") {
    Expect.value(Raindrops.convert(25)).toEqual("Plang")
  }

  skip.test("the sound for 27 is Pling as it has a factor 3") {
    Expect.value(Raindrops.convert(27)).toEqual("Pling")
  }

  skip.test("the sound for 35 is PlangPlong as it has factors 5 and 7") {
    Expect.value(Raindrops.convert(35)).toEqual("PlangPlong")
  }

  skip.test("the sound for 49 is Plong as it has a factor 7") {
    Expect.value(Raindrops.convert(49)).toEqual("Plong")
  }

  skip.test("the sound for 52 is 52") {
    Expect.value(Raindrops.convert(52)).toEqual("52")
  }

  skip.test("the sound for 105 is PlingPlangPlong as it has factors 3, 5 and 7") {
    Expect.value(Raindrops.convert(105)).toEqual("PlingPlangPlong")
  }

  skip.test("the sound for 3125 is Plang as it has a factor 5") {
    Expect.value(Raindrops.convert(3125)).toEqual("Plang")
  }
}
