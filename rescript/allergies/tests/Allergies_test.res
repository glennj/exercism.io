open Test
open Allergies

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("not allergic to anything", () => {
  equal(~message="not allergic to anything", allergicTo(Eggs, 0), false)
})

test("allergic only to eggs", () => {
  equal(~message="allergic only to eggs", allergicTo(Eggs, 1), true)
})

test("allergic to eggs and something else", () => {
  equal(~message="allergic to eggs and something else", allergicTo(Eggs, 3), true)
})

test("allergic to something, but not eggs", () => {
  equal(~message="allergic to something, but not eggs", allergicTo(Eggs, 2), false)
})

test("allergic to everything", () => {
  equal(~message="allergic to everything", allergicTo(Eggs, 255), true)
})

test("not allergic to anything", () => {
  equal(~message="not allergic to anything", allergicTo(Peanuts, 0), false)
})

test("allergic only to peanuts", () => {
  equal(~message="allergic only to peanuts", allergicTo(Peanuts, 2), true)
})

test("allergic to peanuts and something else", () => {
  equal(~message="allergic to peanuts and something else", allergicTo(Peanuts, 7), true)
})

test("allergic to something, but not peanuts", () => {
  equal(~message="allergic to something, but not peanuts", allergicTo(Peanuts, 5), false)
})

test("allergic to everything", () => {
  equal(~message="allergic to everything", allergicTo(Peanuts, 255), true)
})

test("not allergic to anything", () => {
  equal(~message="not allergic to anything", allergicTo(Shellfish, 0), false)
})

test("allergic only to shellfish", () => {
  equal(~message="allergic only to shellfish", allergicTo(Shellfish, 4), true)
})

test("allergic to shellfish and something else", () => {
  equal(~message="allergic to shellfish and something else", allergicTo(Shellfish, 14), true)
})

test("allergic to something, but not shellfish", () => {
  equal(~message="allergic to something, but not shellfish", allergicTo(Shellfish, 10), false)
})

test("allergic to everything", () => {
  equal(~message="allergic to everything", allergicTo(Shellfish, 255), true)
})

test("not allergic to anything", () => {
  equal(~message="not allergic to anything", allergicTo(Strawberries, 0), false)
})

test("allergic only to strawberries", () => {
  equal(~message="allergic only to strawberries", allergicTo(Strawberries, 8), true)
})

test("allergic to strawberries and something else", () => {
  equal(~message="allergic to strawberries and something else", allergicTo(Strawberries, 28), true)
})

test("allergic to something, but not strawberries", () => {
  equal(~message="allergic to something, but not strawberries", allergicTo(Strawberries, 20), false)
})

test("allergic to everything", () => {
  equal(~message="allergic to everything", allergicTo(Strawberries, 255), true)
})

test("not allergic to anything", () => {
  equal(~message="not allergic to anything", allergicTo(Tomatoes, 0), false)
})

test("allergic only to tomatoes", () => {
  equal(~message="allergic only to tomatoes", allergicTo(Tomatoes, 16), true)
})

test("allergic to tomatoes and something else", () => {
  equal(~message="allergic to tomatoes and something else", allergicTo(Tomatoes, 56), true)
})

test("allergic to something, but not tomatoes", () => {
  equal(~message="allergic to something, but not tomatoes", allergicTo(Tomatoes, 40), false)
})

test("allergic to everything", () => {
  equal(~message="allergic to everything", allergicTo(Tomatoes, 255), true)
})

test("not allergic to anything", () => {
  equal(~message="not allergic to anything", allergicTo(Chocolate, 0), false)
})

test("allergic only to chocolate", () => {
  equal(~message="allergic only to chocolate", allergicTo(Chocolate, 32), true)
})

test("allergic to chocolate and something else", () => {
  equal(~message="allergic to chocolate and something else", allergicTo(Chocolate, 112), true)
})

test("allergic to something, but not chocolate", () => {
  equal(~message="allergic to something, but not chocolate", allergicTo(Chocolate, 80), false)
})

test("allergic to everything", () => {
  equal(~message="allergic to everything", allergicTo(Chocolate, 255), true)
})

test("not allergic to anything", () => {
  equal(~message="not allergic to anything", allergicTo(Pollen, 0), false)
})

test("allergic only to pollen", () => {
  equal(~message="allergic only to pollen", allergicTo(Pollen, 64), true)
})

test("allergic to pollen and something else", () => {
  equal(~message="allergic to pollen and something else", allergicTo(Pollen, 224), true)
})

test("allergic to something, but not pollen", () => {
  equal(~message="allergic to something, but not pollen", allergicTo(Pollen, 160), false)
})

test("allergic to everything", () => {
  equal(~message="allergic to everything", allergicTo(Pollen, 255), true)
})

test("not allergic to anything", () => {
  equal(~message="not allergic to anything", allergicTo(Cats, 0), false)
})

test("allergic only to cats", () => {
  equal(~message="allergic only to cats", allergicTo(Cats, 128), true)
})

test("allergic to cats and something else", () => {
  equal(~message="allergic to cats and something else", allergicTo(Cats, 192), true)
})

test("allergic to something, but not cats", () => {
  equal(~message="allergic to something, but not cats", allergicTo(Cats, 64), false)
})

test("allergic to everything", () => {
  equal(~message="allergic to everything", allergicTo(Cats, 255), true)
})

test("no allergies", () => {equal(~message="no allergies", list(0), [])})

test("just eggs", () => {equal(~message="just eggs", list(1), [Eggs])})

test("just peanuts", () => {equal(~message="just peanuts", list(2), [Peanuts])})

test("just strawberries", () => {equal(~message="just strawberries", list(8), [Strawberries])})

test("eggs and peanuts", () => {equal(~message="eggs and peanuts", list(3), [Eggs, Peanuts])})

test("more than eggs but not peanuts", () => {
  equal(~message="more than eggs but not peanuts", list(5), [Eggs, Shellfish])
})

test("lots of stuff", () => {
  equal(~message="lots of stuff", list(248), [Strawberries, Tomatoes, Chocolate, Pollen, Cats])
})

test("everything", () => {
  equal(
    ~message="everything",
    list(255),
    [Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats],
  )
})

test("no allergen score parts", () => {
  equal(
    ~message="no allergen score parts",
    list(509),
    [Eggs, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats],
  )
})

test("no allergen score parts without highest valid score", () => {
  equal(~message="no allergen score parts without highest valid score", list(257), [Eggs])
})
