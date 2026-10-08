open Test
open FlowerField

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("no rows", () => {equal(~message="no rows", annotate([]), [])})

test("no columns", () => {equal(~message="no columns", annotate([""]), [""])})

test("no flowers", () => {
  equal(~message="no flowers", annotate(["   ", "   ", "   "]), ["   ", "   ", "   "])
})

test("garden full of flowers", () => {
  equal(~message="garden full of flowers", annotate(["***", "***", "***"]), ["***", "***", "***"])
})

test("flower surrounded by spaces", () => {
  equal(
    ~message="flower surrounded by spaces",
    annotate(["   ", " * ", "   "]),
    ["111", "1*1", "111"],
  )
})

test("space surrounded by flowers", () => {
  equal(
    ~message="space surrounded by flowers",
    annotate(["***", "* *", "***"]),
    ["***", "*8*", "***"],
  )
})

test("horizontal line", () => {equal(~message="horizontal line", annotate([" * * "]), ["1*2*1"])})

test("horizontal line, flowers at edges", () => {
  equal(~message="horizontal line, flowers at edges", annotate(["*   *"]), ["*1 1*"])
})

test("vertical line", () => {
  equal(~message="vertical line", annotate([" ", "*", " ", "*", " "]), ["1", "*", "2", "*", "1"])
})

test("vertical line, flowers at edges", () => {
  equal(
    ~message="vertical line, flowers at edges",
    annotate(["*", " ", " ", " ", "*"]),
    ["*", "1", " ", "1", "*"],
  )
})

test("cross", () => {
  equal(
    ~message="cross",
    annotate(["  *  ", "  *  ", "*****", "  *  ", "  *  "]),
    [" 2*2 ", "25*52", "*****", "25*52", " 2*2 "],
  )
})

test("large garden", () => {
  equal(
    ~message="large garden",
    annotate([" *  * ", "  *   ", "    * ", "   * *", " *  * ", "      "]),
    ["1*22*1", "12*322", " 123*2", "112*4*", "1*22*2", "111111"],
  )
})

test("multiple adjacent flowers", () => {
  equal(~message="multiple adjacent flowers", annotate([" ** "]), ["1**1"])
})
