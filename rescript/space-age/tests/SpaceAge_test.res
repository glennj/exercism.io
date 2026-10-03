open Test
open SpaceAge

let floatApproximatelyEqual = (~message=?, a, b) =>
  assertion(~message?, ~operator="floatApprox", (a, b) => Math.abs(a -. b) <= 2., a, b)

test("age on Earth", () => {
  floatApproximatelyEqual(~message="age on Earth", age(Earth, 1000000000), 31.69)
})

test("age on Mercury", () => {
  floatApproximatelyEqual(~message="age on Mercury", age(Mercury, 2134835688), 280.88)
})

test("age on Venus", () => {
  floatApproximatelyEqual(~message="age on Venus", age(Venus, 189839836), 9.78)
})

test("age on Mars", () => {
  floatApproximatelyEqual(~message="age on Mars", age(Mars, 2129871239), 35.88)
})

test("age on Jupiter", () => {
  floatApproximatelyEqual(~message="age on Jupiter", age(Jupiter, 901876382), 2.41)
})

test("age on Saturn", () => {
  floatApproximatelyEqual(~message="age on Saturn", age(Saturn, 2000000000), 2.15)
})

test("age on Uranus", () => {
  floatApproximatelyEqual(~message="age on Uranus", age(Uranus, 1210123456), 0.46)
})

test("age on Neptune", () => {
  floatApproximatelyEqual(~message="age on Neptune", age(Neptune, 1821023456), 0.35)
})
