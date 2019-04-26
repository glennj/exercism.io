triangle <- function(x, y, z) {
  t <- sort(c(x, y, z))
  if (t[1] <= 0 || t[1] + t[2] <= t[3]) {
    stop("not a triangle")
  }
  class(t) <- append(class(t),
    if (t[1] == t[2] && t[1] == t[3]) {
      c("equilateral", "isosceles")
    } else if (t[1] == t[2] || t[2] == t[3]) {
      "isosceles"
    } else {
      "scalene"
    }
  )
  t
}
