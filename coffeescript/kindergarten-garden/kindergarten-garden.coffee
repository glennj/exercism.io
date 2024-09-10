class KindergartenGarden
  Plants = {R: "radishes", C: "clover", G: "grass", V: "violets"}
  Students = ['Alice', 'Bob', 'Charlie', 'David', 'Eve', 'Fred',
              'Ginny', 'Harriet', 'Ileana', 'Joseph', 'Kincaid', 'Larry']

  constructor: (diagram) ->
    @studentPlots = diagram
                      .split("\n")
                      .map (line) -> line.match /../g
                      .transpose()
                      .map (pair) -> [pair.join('')...].map (p) -> Plants[p]

  plants: (student) -> 
    idx = Students.indexOf student
    @studentPlots[idx] if idx != -1

module.exports = KindergartenGarden


Array::transpose = ->
  result = []
  for i in [0 ... this[0].length]
    result[i] = []
    for j in [0 ... this.length]
      result[i][j] = this[j][i]
  result
