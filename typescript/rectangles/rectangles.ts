type Point = {x: number, y: number}
// a rectangle can be defined by its upper-left and lower-right vertices
type Rectangle = [Point, Point]

export const count = (lines: string[]): number => {
  return Rectangles.count(lines)
}

class Rectangles {
  static count(lines: string[]): number {
    return lines.length === 0 ? 0 : this.findRectangles(lines).length
  }

  private static findRectangles (rows: string[]): Rectangle[] {
    const rectangles: Rectangle[] = []

    // transpose the input array
    const columns: string[] = Array.from(rows[0]).map(
      (_, i) => rows.map((row) => row[i]).join('')
    )
    // find the vertices within the input array ("+" characters)
    const vertices: Point[] = this.findVertices(rows)

    for (let r = 0; r < rows.length; r += 1) {
      const verticesInRow = vertices.filter((v) => v.y === r)
      const xPairs = pairs( verticesInRow.map((v) => v.x) )

      for (const [x1, x2] of xPairs) {
        // vertices in column x1 _below_ row r
        const inCol1BelowR = vertices.filter((v) => v.x === x1 && v.y > r)

        for (const y of inCol1BelowR.map((v) => v.y)) {
          // I have a vertex [x1, y]: is [x2, y] a vertex?
          if (this.isVertex(vertices, {x: x2, y})) {
            const rect: Rectangle = [{x: x1, y: r}, {x: x2, y}]
            if (this.isComplete(rows, columns, rect)) {
              rectangles.push(rect)
            }
          }
        }
      }
    }
    return rectangles
  }

  // find the coordinates of all the "+" chars in the input text
  private static findVertices (lines: string[]): Point[] {
    const vertices: Point[] = []
    lines.forEach((row, y) => {
      row.split('').forEach((char, x) => {
        if (char === '+') { vertices.push({x, y}) }
      })
    })
    return vertices
  }

  private static isVertex(vertices: Point[], p: Point): boolean {
    // unfortunately, this does not work:  vertices.includes(p)
    // because {x:0,y:0} === {x:0,y:0} is false
    return vertices
      .filter((v) => v.x === p.x && v.y === p.y)
      .length > 0
  }

  private static horizontalEdge = new RegExp(/^[+-]+$/) // legal chars for horizontal edge
  private static verticalEdge   = new RegExp(/^[+|]+$/) // legal chars for vertical edge

  // ensure the potential rectangle has "complete" sides, i.e. no gaps
  private static isComplete (
      rows: string[],
      columns: string[],
      [p1, p2]: Rectangle
  ): boolean {
    return this.horizontalEdge.test(rows[p1.y].slice(p1.x, p2.x))   // top edge
        && this.horizontalEdge.test(rows[p2.y].slice(p1.x, p2.x))   // bottom edge
        && this.verticalEdge.test(columns[p1.x].slice(p1.y, p2.y))  // left edge
        && this.verticalEdge.test(columns[p2.x].slice(p1.y, p2.y))  // right edge
  }
}


// [a, b, c] => [[a, b], [a, c], [b, c]]
function pairs<T> (list: T[]): T[][] {
  const pairs: T[][] = []
  list.forEach((val1, i) => {
    list.slice(i + 1).map( (val2) => pairs.push([val1, val2]) )
  })
  return pairs
}
