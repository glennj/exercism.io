fun hypot (x: real, y: real): real = Math.sqrt(x * x + y * y)

fun score (x: real, y: real): int =
  let val distance = hypot(x, y)
      val circleValues = [(1.0, 10), (5.0, 5), (10.0, 1)]
  in  case List.find (fn (radius, _) => distance <= radius) circleValues
        of SOME (_, score) => score
         | NONE            => 0
  end
