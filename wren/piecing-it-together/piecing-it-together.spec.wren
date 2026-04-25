import "./piecing-it-together" for PiecingItTogether
import "wren-testie/testie" for Testie, Expect

Testie.test("PiecingItTogether") { |do, skip|
  do.test("1000 pieces puzzle with 1.6 aspect ratio") {
    var partial = {"pieces": 1000, "aspectRatio": 1.6}
    var expected = {"pieces": 1000, "border": 126, "inside": 874, "rows": 25, "columns": 40, "aspectRatio": 1.6, "format": "landscape"}
    var received = PiecingItTogether.jigsawData(partial)
    Expect.value(received).toEqual(expected)
  }

  do.test("square puzzle with 32 rows") {
    var partial = {"rows": 32, "format": "square"}
    var expected = {"pieces": 1024, "border": 124, "inside": 900, "rows": 32, "columns": 32, "aspectRatio": 1.0, "format": "square"}
    var received = PiecingItTogether.jigsawData(partial)
    Expect.value(received).toEqual(expected)
  }

  do.test("400 pieces square puzzle with only inside pieces and aspect ratio") {
    var partial = {"inside": 324, "aspectRatio": 1.0}
    var expected = {"pieces": 400, "border": 76, "inside": 324, "rows": 20, "columns": 20, "aspectRatio": 1.0, "format": "square"}
    var received = PiecingItTogether.jigsawData(partial)
    Expect.value(received).toEqual(expected)
  }

  do.test("1500 pieces landscape puzzle with 30 rows and 1.6 aspect ratio") {
    var partial = {"rows": 30, "aspectRatio": 1.6666666666666667}
    var expected = {"pieces": 1500, "border": 156, "inside": 1344, "rows": 30, "columns": 50, "aspectRatio": 1.6666666666666667, "format": "landscape"}
    var received = PiecingItTogether.jigsawData(partial)
    Expect.value(received).toEqual(expected)
  }

  do.test("300 pieces portrait puzzle with 70 border pieces") {
    var partial = {"pieces": 300, "border": 70, "format": "portrait"}
    var expected = {"pieces": 300, "border": 70, "inside": 230, "rows": 25, "columns": 12, "aspectRatio": 0.48, "format": "portrait"}
    var received = PiecingItTogether.jigsawData(partial)
    Expect.value(received).toEqual(expected)
  }

  do.test("puzzle with insufficient data") {
    var partial = {"pieces": 1500, "format": "landscape"}
    Expect.that {
      PiecingItTogether.jigsawData(partial)
    }.abortsWith("Insufficient data")
  }

  do.test("puzzle with contradictory data") {
    var partial = {"rows": 100, "columns": 1000, "format": "square"}
    Expect.that {
      PiecingItTogether.jigsawData(partial)
    }.abortsWith("Contradictory data")
  }
}
