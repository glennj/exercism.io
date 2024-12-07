fun mmi (a: int, m: int, x: int): int = 
  if m = x then 0
  else if (a * x) mod m = 1 then x
  else mmi (a, m, x + 1)

fun gcd (a: int, b: int): int =
  if b = 0 then a else gcd(b, a mod b)

fun hypot (x: real, y: real): real =
  Math.sqrt(x * x + y * y)
