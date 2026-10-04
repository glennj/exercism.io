// straightforward arithmetic solution
/*
let squareOfSum  = n => (n * (n + 1) / 2) ** 2
let sumOfSquares = n => n * (n + 1) * (2 * n + 1) / 6
*/

// less efficient but more fun functional solution

let ident  = n => n
let square = n => n ** 2

let sum = (n, ~inner=ident, ~outer=ident) =>
  Array.fromInitializer(~length=n, i => i + 1)
  ->Array.reduce(0, (acc, elem) => acc + inner(elem))
  ->outer

let squareOfSum  = n => sum(n, ~outer=square)
let sumOfSquares = n => sum(n, ~inner=square)

let differenceOfSquares = n => squareOfSum(n) - sumOfSquares(n)
