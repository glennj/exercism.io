open Test
open MatchingBrackets

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("paired square brackets", () => {
  equal(~message="paired square brackets", isPaired("[]"), true)
})

test("empty string", () => {equal(~message="empty string", isPaired(""), true)})

test("unpaired brackets", () => {equal(~message="unpaired brackets", isPaired("[["), false)})

test("wrong ordered brackets", () => {
  equal(~message="wrong ordered brackets", isPaired("}{"), false)
})

test("wrong closing bracket", () => {
  equal(~message="wrong closing bracket", isPaired("{]"), false)
})

test("paired with whitespace", () => {
  equal(~message="paired with whitespace", isPaired("{ }"), true)
})

test("partially paired brackets", () => {
  equal(~message="partially paired brackets", isPaired("{[])"), false)
})

test("simple nested brackets", () => {
  equal(~message="simple nested brackets", isPaired("{[]}"), true)
})

test("several paired brackets", () => {
  equal(~message="several paired brackets", isPaired("{}[]"), true)
})

test("paired and nested brackets", () => {
  equal(~message="paired and nested brackets", isPaired("([{}({}[])])"), true)
})

test("unopened closing brackets", () => {
  equal(~message="unopened closing brackets", isPaired("{[)][]}"), false)
})

test("unpaired and nested brackets", () => {
  equal(~message="unpaired and nested brackets", isPaired("([{])"), false)
})

test("paired and wrong nested brackets", () => {
  equal(~message="paired and wrong nested brackets", isPaired("[({]})"), false)
})

test("paired and wrong nested brackets but innermost are correct", () => {
  equal(
    ~message="paired and wrong nested brackets but innermost are correct",
    isPaired("[({}])"),
    false,
  )
})

test("paired and incomplete brackets", () => {
  equal(~message="paired and incomplete brackets", isPaired("{}["), false)
})

test("too many closing brackets", () => {
  equal(~message="too many closing brackets", isPaired("[]]"), false)
})

test("early unexpected brackets", () => {
  equal(~message="early unexpected brackets", isPaired(")()"), false)
})

test("early mismatched brackets", () => {
  equal(~message="early mismatched brackets", isPaired("{)()"), false)
})

test("math expression", () => {
  equal(~message="math expression", isPaired("(((185 + 223.85) * 15) - 543)/2"), true)
})

test("complex latex expression", () => {
  equal(
    ~message="complex latex expression",
    isPaired(
      "\\left(\\begin{array}{cc} \\frac{1}{3} & x\\\\ \\mathrm{e}^{x} &... x^2 \\end{array}\\right)",
    ),
    true,
  )
})
