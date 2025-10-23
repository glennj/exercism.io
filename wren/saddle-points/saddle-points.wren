import "./math" for Math

class Matrix {
  construct new(values) {
    if (values.isEmpty) Fiber.abort("must have at least 1 row")

    _mtx = values
    _height = values.count
    _width = values[0].count
  }

  saddlePoints() {
    var rowMaxima = List.filled(_height, Num.smallest)
    var colMinima = List.filled(_width, Num.largest)

    for (i in 0..._height) {
      for (j in 0..._width) {
        rowMaxima[i] = Math.max(rowMaxima[i], _mtx[i][j])
        colMinima[j] = Math.min(colMinima[j], _mtx[i][j])
      }
    }

    var saddlePoints = []
    for (i in 0..._height) {
      for (j in 0..._width) {
        if (_mtx[i][j] == rowMaxima[i] && _mtx[i][j] == colMinima[j]) {
          saddlePoints.add({"row": i + 1, "column": j + 1})
        }
      }
    }
    return saddlePoints
  }
}
