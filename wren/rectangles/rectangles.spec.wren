import "wren-testie/testie" for Testie, Expect
import "./rectangles" for Rectangles

Testie.test("Rectangles") { |do, skip|
  do.test("no rows") {
    var r = Rectangles.new([])
    Expect.value(r.count).toEqual(0)
  }
  skip.test("no columns") {
    var r = Rectangles.new([""])
    Expect.value(r.count).toEqual(0)
  }
  skip.test("no rectangles") {
    var r = Rectangles.new([" "])
    Expect.value(r.count).toEqual(0)
  }

  skip.test("one rectangle") {
    var r = Rectangles.new([
      "+-+",
      "| |",
      "+-+"
    ])
    Expect.value(r.count).toEqual(1)
  }

  skip.test("two rectangles without shared parts") {
    var r = Rectangles.new([
      "  +-+",
      "  | |",
      "+-+-+",
      "| |  ",
      "+-+  "
    ])
    Expect.value(r.count).toEqual(2)
  }

  skip.test("five rectangles with shared parts") {
    var r = Rectangles.new([
      "  +-+",
      "  | |",
      "+-+-+",
      "| | |",
      "+-+-+"
    ])
    Expect.value(r.count).toEqual(5)
  }

  skip.test("rectangle of height 1 is counted") {
    var r = Rectangles.new([
      "+--+",
      "+--+"
    ])
    Expect.value(r.count).toEqual(1)
  }

  skip.test("rectangle of width 1 is counted") {
    var r = Rectangles.new([
      "++",
      "||",
      "++"
    ])
    Expect.value(r.count).toEqual(1)
  }

  skip.test("1x1 square is counted") {
    var r = Rectangles.new([
      "++",
      "++"
    ])
    Expect.value(r.count).toEqual(1)
  }

  skip.test("rectangles must have four sides") {
    var r = Rectangles.new([
      "+-+ +-+",
      "| | | |",
      "+-+-+-+",
      "  | |  ",
      "+-+-+-+",
      "| | | |",
      "+-+ +-+"
    ])
    Expect.value(r.count).toEqual(5)
  }

  skip.test("only complete rectangles are counted") {
    var r = Rectangles.new([
      "  +-+",
      "    |",
      "+-+-+",
      "| | -",
      "+-+-+"
    ])
    Expect.value(r.count).toEqual(1)
  }

  skip.test("rectangles can be of different sizes") {
    var r = Rectangles.new([
      "+------+----+",
      "|      |    |",
      "+---+--+    |",
      "|   |       |",
      "+---+-------+"
    ])
    Expect.value(r.count).toEqual(3)
  }

  skip.test("corner is required for a rectangle to be complete") {
    var r = Rectangles.new([
      "+------+----+",
      "|      |    |",
      "+------+    |",
      "|   |       |",
      "+---+-------+"
    ])
    Expect.value(r.count).toEqual(2)
  }

  skip.test("large input with many rectangles") {
    var r = Rectangles.new([
      "+---+--+----+",
      "|   +--+----+",
      "+---+--+    |",
      "|   +--+----+",
      "+---+--+--+-+",
      "+---+--+--+-+",
      "+------+  | |",
      "          +-+"
    ])
    Expect.value(r.count).toEqual(60)
  }
}
