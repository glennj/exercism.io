type point = {x: float, y: float, angle: float}
type prism = {id: int, point: point}

let deg2rad = degrees => degrees * Math.Constants.pi / 180.0
let cos = degrees => degrees->deg2rad->Math.cos
let sin = degrees => degrees->deg2rad->Math.sin

let isInline = (p: point, prism: prism): bool => {
  // Determine the _perpendicular_ distance of the prism from the point's line
  let dist = (prism.point.x - p.x) * sin(p.angle) - (prism.point.y - p.y) * cos(p.angle)
  // If it's on the line, dist should be zero, but floating point math...
  // Particularly with the accumulated angle's precision.
  // Test it against some epsilon (determined by trial and error).
  Math.abs(dist) <= 0.0011
}

// Given a list of prisms that are inline with the current point,
// find the closest one.
let nextPrism = (p: point, prisms: array<prism>): result<prism, unit> => {
  let rec finder = (ps, nextP, min) => {
    switch ps {
    | list{} => nextP
    | list{prism, ...rest} => {
        let dist = (prism.point.x - p.x) * cos(p.angle) + (prism.point.y - p.y) * sin(p.angle)
        if 0.0 < dist && dist < min {
          finder(rest, Ok(prism), dist)
        } else {
          finder(rest, nextP, min)
        }
      }
    }
  }
  finder(prisms->List.fromArray, Error(), Float.Constants.maxValue)
}

let findSequence = (start: point, prisms: array<prism>) => {
  let p = ref(start)
  let seq = []
  let looping = ref(true)

  while looping.contents {
    let inlinePrisms = prisms->Array.filter(prism => isInline(p.contents, prism))
    switch nextPrism(p.contents, inlinePrisms) {
    | Error(_) => looping := false
    | Ok(next) => {
        Array.push(seq, next.id)
        p := {...next.point, angle: p.contents.angle + next.point.angle}
        // Note, not necessary to normalize the angle to range (0..360):
        // cos(90) == cos(90 + 360)
      }
    }
  }
  seq
}
