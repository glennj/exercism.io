type garden = array<string>
type matrix = array<array<int>>
type point = {row: int, col: int}

// ----------------------------------------------
// some general 2D matrix functions

let getAt = (m: matrix, p: point): int =>
  m->Array.getUnsafe(p.row)->Array.getUnsafe(p.col)

let setAt = (m: matrix, p: point, val: int): unit =>
  m->Array.getUnsafe(p.row)->Array.set(p.col, val)

let ht = (m: matrix): int => m->Array.length
let wd = (m: matrix): int => m->Array.getUnsafe(0)->Array.length

let inRange = (m: matrix, p: point): bool =>
  0 <= p.row && p.row < ht(m) && (0 <= p.col && p.col < wd(m))

// ----------------------------------------------
// utility functions for this exercise

let flower = 99 // magic sentinel value
let isFlower = n => n === flower

let deltas = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

let countNeighbours = (m: matrix, p: point): int => {
  deltas->Array.reduce(0, (count, (dr, dc)) => {
    let q = {row: p.row + dr, col: p.col + dc}
    if m->inRange(q) && m->getAt(q)->isFlower {
      count + 1
    } else {
      count
    }
  })
}

let garden2matrix = (g: garden): matrix =>
  g->Array.map(line =>
    line
    ->String.split("")
    ->Array.map(c =>
      switch c {
      | "*" => flower
      | _ => 0
      }
    )
  )

let matrix2garden = (m: matrix): garden =>
  m->Array.map(row =>
    row->Array.map(count =>
      // can't use a switch here: putting a name in the
      // pattern will be seen as a destructuring assigmnent
      if count === flower {
        "*"
      } else if count === 0 {
        " "
      } else {
        Int.toString(count)
      }
    )->Array.join("")
  )

// ----------------------------------------------
let annotate = (g: garden): garden => {
  let mtx = garden2matrix(g)

  for r in 0 to ht(mtx) - 1 {
    for c in 0 to wd(mtx) - 1 {
      let p = {row: r, col: c}
      if !(mtx->getAt(p)->isFlower) {
        mtx->setAt(p, mtx->countNeighbours(p))
      }
    }
  }

  matrix2garden(mtx)
}
