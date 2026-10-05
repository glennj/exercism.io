open Test
open AllYourBase

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("single bit one to decimal", () => {
  equal(~message="single bit one to decimal", rebase(2, [1], 10), Some([1]))
})

test("binary to single decimal", () => {
  equal(~message="binary to single decimal", rebase(2, [1, 0, 1], 10), Some([5]))
})

test("single decimal to binary", () => {
  equal(~message="single decimal to binary", rebase(10, [5], 2), Some([1, 0, 1]))
})

test("binary to multiple decimal", () => {
  equal(~message="binary to multiple decimal", rebase(2, [1, 0, 1, 0, 1, 0], 10), Some([4, 2]))
})

test("decimal to binary", () => {
  equal(~message="decimal to binary", rebase(10, [4, 2], 2), Some([1, 0, 1, 0, 1, 0]))
})

test("trinary to hexadecimal", () => {
  equal(~message="trinary to hexadecimal", rebase(3, [1, 1, 2, 0], 16), Some([2, 10]))
})

test("hexadecimal to trinary", () => {
  equal(~message="hexadecimal to trinary", rebase(16, [2, 10], 3), Some([1, 1, 2, 0]))
})

test("15-bit integer", () => {
  equal(~message="15-bit integer", rebase(97, [3, 46, 60], 73), Some([6, 10, 45]))
})

test("empty list", () => {equal(~message="empty list", rebase(2, [], 10), Some([0]))})

test("single zero", () => {equal(~message="single zero", rebase(10, [0], 2), Some([0]))})

test("multiple zeros", () => {
  equal(~message="multiple zeros", rebase(10, [0, 0, 0], 2), Some([0]))
})

test("leading zeros", () => {
  equal(~message="leading zeros", rebase(7, [0, 6, 0], 10), Some([4, 2]))
})

test("input base is one", () => {equal(~message="input base is one", rebase(1, [0], 10), None)})

test("input base is zero", () => {equal(~message="input base is zero", rebase(0, [], 10), None)})

test("input base is negative", () => {
  equal(~message="input base is negative", rebase(-2, [1], 10), None)
})

test("negative digit", () => {
  equal(~message="negative digit", rebase(2, [1, -1, 1, 0, 1, 0], 10), None)
})

test("invalid positive digit", () => {
  equal(~message="invalid positive digit", rebase(2, [1, 2, 1, 0, 1, 0], 10), None)
})

test("output base is one", () => {
  equal(~message="output base is one", rebase(2, [1, 0, 1, 0, 1, 0], 1), None)
})

test("output base is zero", () => {equal(~message="output base is zero", rebase(10, [7], 0), None)})

test("output base is negative", () => {
  equal(~message="output base is negative", rebase(2, [1], -7), None)
})

test("both bases are negative", () => {
  equal(~message="both bases are negative", rebase(-2, [1], -7), None)
})
