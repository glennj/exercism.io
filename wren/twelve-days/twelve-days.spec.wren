import "wren-testie/testie" for Testie, Expect
import "./twelve-days" for TwelveDays

Testie.test("Twelve Days") { |do, skip|
  do.test("test verse first day a partridge in a pear tree") {
    var expectedVerseOne =
      "On the first day of Christmas my true love gave to me: a Partridge in a Pear Tree.\n"
    Expect.value(TwelveDays.recite(1)).toEqual(expectedVerseOne)
  }

  skip.test("test verse second day two turtle doves") {
    var expectedVerseTwo =
      "On the second day of Christmas my true love gave to me: two Turtle Doves, and a Partridge in a Pear Tree.\n"
    Expect.value(TwelveDays.recite(2)).toEqual(expectedVerseTwo)
  }

  skip.test("test verse third day three french hens") {
    var expectedVerseThree =
      "On the third day of Christmas my true love gave to me: three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n"
    Expect.value(TwelveDays.recite(3)).toEqual(expectedVerseThree)
  }

  skip.test("test verse fourth day four calling birds") {
    var expectedVerseFour =
      "On the fourth day of Christmas my true love gave to me: four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n"
    Expect.value(TwelveDays.recite(4)).toEqual(expectedVerseFour)
  }

  skip.test("test verse fifth day five gold rings") {
    var expectedVerseFive =
      "On the fifth day of Christmas my true love gave to me: five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n"
    Expect.value(TwelveDays.recite(5)).toEqual(expectedVerseFive)
  }

  skip.test("test verse sixth day six geese-a-laying") {
    var expectedVerseSix =
      "On the sixth day of Christmas my true love gave to me: six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n"
    Expect.value(TwelveDays.recite(6)).toEqual(expectedVerseSix)
  }

  skip.test("test verse seventh day seven swans-a-swimming") {
    var expectedVerseSeven =
      "On the seventh day of Christmas my true love gave to me: seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n"
    Expect.value(TwelveDays.recite(7)).toEqual(expectedVerseSeven)
  }

  skip.test("test verse eighth day eight maids-a-milking") {
    var expectedVerseEight =
      "On the eighth day of Christmas my true love gave to me: eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n"
    Expect.value(TwelveDays.recite(8)).toEqual(expectedVerseEight)
  }

  skip.test("test verse ninth day nine ladies dancing") {
    var expectedVerseNine =
      "On the ninth day of Christmas my true love gave to me: nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n"
    Expect.value(TwelveDays.recite(9)).toEqual(expectedVerseNine)
  }

  skip.test("test verse tenth day ten lords-a-leaping") {
    var expectedVerseTen =
      "On the tenth day of Christmas my true love gave to me: ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n"
    Expect.value(TwelveDays.recite(10)).toEqual(expectedVerseTen)
  }

  skip.test("test verse eleventh day eleven pipers piping") {
    var expectedVerseEleven =
      "On the eleventh day of Christmas my true love gave to me: eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n"
    Expect.value(TwelveDays.recite(11)).toEqual(expectedVerseEleven)
  }

  skip.test("test verse twelfth day twelve drummers drumming") {
    var expectedVerseTwelve =
      "On the twelfth day of Christmas my true love gave to me: twelve Drummers Drumming, eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n"
    Expect.value(TwelveDays.recite(12)).toEqual(expectedVerseTwelve)
  }

  skip.test("test lyrics TwelveDays.recites first three verses of the song") {
    var expectedVerseOneToThree =
      "On the first day of Christmas my true love gave to me: a Partridge in a Pear Tree.\n\n" +
      "On the second day of Christmas my true love gave to me: two Turtle Doves, and a Partridge in a Pear Tree.\n\n" +
      "On the third day of Christmas my true love gave to me: three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n"
    Expect.value(TwelveDays.recite(1, 3)).toEqual(expectedVerseOneToThree)
  }

  skip.test("test lyrics TwelveDays.recites three verses from the middle of the song") {
    var expectedVerseFourToSix =
      "On the fourth day of Christmas my true love gave to me: four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n\n" +
      "On the fifth day of Christmas my true love gave to me: five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n\n" +
      "On the sixth day of Christmas my true love gave to me: six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n"
    Expect.value(TwelveDays.recite(4, 6)).toEqual(expectedVerseFourToSix)
  }

  skip.test("test lyrics TwelveDays.recites the whole song") {
    var expectedSong =
      "On the first day of Christmas my true love gave to me: a Partridge in a Pear Tree.\n\n" +
      "On the second day of Christmas my true love gave to me: two Turtle Doves, and a Partridge in a Pear Tree.\n\n" +
      "On the third day of Christmas my true love gave to me: three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n\n" +
      "On the fourth day of Christmas my true love gave to me: four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n\n" +
      "On the fifth day of Christmas my true love gave to me: five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n\n" +
      "On the sixth day of Christmas my true love gave to me: six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n\n" +
      "On the seventh day of Christmas my true love gave to me: seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n\n" +
      "On the eighth day of Christmas my true love gave to me: eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n\n" +
      "On the ninth day of Christmas my true love gave to me: nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n\n" +
      "On the tenth day of Christmas my true love gave to me: ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n\n" +
      "On the eleventh day of Christmas my true love gave to me: eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n\n" +
      "On the twelfth day of Christmas my true love gave to me: twelve Drummers Drumming, eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, and a Partridge in a Pear Tree.\n"
    Expect.value(TwelveDays.recite(1, 12)).toEqual(expectedSong)
  }
}
