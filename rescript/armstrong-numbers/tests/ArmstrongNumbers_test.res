open Test
open ArmstrongNumbers

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("Zero is an Armstrong number", () => {
  equal(~message="Zero is an Armstrong number", isArmstrongNumber(0), true)
})

test("Single-digit numbers are Armstrong numbers", () => {
  equal(~message="Single-digit numbers are Armstrong numbers", isArmstrongNumber(5), true)
})

test("There are no two-digit Armstrong numbers", () => {
  equal(~message="There are no two-digit Armstrong numbers", isArmstrongNumber(10), false)
})

test("Three-digit number that is an Armstrong number", () => {
  equal(~message="Three-digit number that is an Armstrong number", isArmstrongNumber(153), true)
})

test("Three-digit number that is not an Armstrong number", () => {
  equal(
    ~message="Three-digit number that is not an Armstrong number",
    isArmstrongNumber(100),
    false,
  )
})

test("Four-digit number that is an Armstrong number", () => {
  equal(~message="Four-digit number that is an Armstrong number", isArmstrongNumber(9474), true)
})

test("Four-digit number that is not an Armstrong number", () => {
  equal(
    ~message="Four-digit number that is not an Armstrong number",
    isArmstrongNumber(9475),
    false,
  )
})

test("Seven-digit number that is an Armstrong number", () => {
  equal(~message="Seven-digit number that is an Armstrong number", isArmstrongNumber(9926315), true)
})

test("Seven-digit number that is not an Armstrong number", () => {
  equal(
    ~message="Seven-digit number that is not an Armstrong number",
    isArmstrongNumber(9926314),
    false,
  )
})
