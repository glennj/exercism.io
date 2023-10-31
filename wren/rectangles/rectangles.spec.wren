import "wren-testie/testie" for Testie, Expect
import "./rectangles" for Rectangles

Testie.test("Rectangles") { |do, skip|
  do.test("no rows") {
    var r = Rectangles.new([])
    Expect.value(r.count).toEqual(0)
  }
  do.test("no columns") {
    var r = Rectangles.new([""])
    Expect.value(r.count).toEqual(0)
  }
  do.test("no rectangles") {
    var r = Rectangles.new([" "])
    Expect.value(r.count).toEqual(0)
  }

  do.test("one rectangle") {
    var r = Rectangles.new([
      "+-+",
      "| |",
      "+-+"
    ])
    Expect.value(r.count).toEqual(1)
  }

  do.test("two rectangles without shared parts") {
    var r = Rectangles.new([
      "  +-+",
      "  | |",
      "+-+-+",
      "| |  ",
      "+-+  "
    ])
    Expect.value(r.count).toEqual(2)
  }

  do.test("five rectangles with shared parts") {
    var r = Rectangles.new([
      "  +-+",
      "  | |",
      "+-+-+",
      "| | |",
      "+-+-+"
    ])
    Expect.value(r.count).toEqual(5)
  }

  do.test("rectangle of height 1 is counted") {
    var r = Rectangles.new([
      "+--+",
      "+--+"
    ])
    Expect.value(r.count).toEqual(1)
  }

  do.test("rectangle of width 1 is counted") {
    var r = Rectangles.new([
      "++",
      "||",
      "++"
    ])
    Expect.value(r.count).toEqual(1)
  }

  do.test("1x1 square is counted") {
    var r = Rectangles.new([
      "++",
      "++"
    ])
    Expect.value(r.count).toEqual(1)
  }

  do.test("rectangles must have four sides") {
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

  do.test("only complete rectangles are counted") {
    var r = Rectangles.new([
      "  +-+",
      "    |",
      "+-+-+",
      "| | -",
      "+-+-+"
    ])
    Expect.value(r.count).toEqual(1)
  }

  do.test("rectangles can be of different sizes") {
    var r = Rectangles.new([
      "+------+----+",
      "|      |    |",
      "+---+--+    |",
      "|   |       |",
      "+---+-------+"
    ])
    Expect.value(r.count).toEqual(3)
  }

  do.test("corner is required for a rectangle to be complete") {
    var r = Rectangles.new([
      "+------+----+",
      "|      |    |",
      "+------+    |",
      "|   |       |",
      "+---+-------+"
    ])
    Expect.value(r.count).toEqual(2)
  }

  do.test("large input with many rectangles") {
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
