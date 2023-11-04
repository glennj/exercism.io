class KindergartenGarden {
  static plants(diagram, student) {
    return new(diagram).plantsFor(student)
  }

  // class variables
  static initialize_() {
    __students = [
      "Alice", "Bob", "Charlie", "David", "Eve", "Fred", 
      "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"
    ]
    __plants = {
      "C": "clover",
      "G": "grass",
      "R": "radishes",
      "V": "violets"
    }
  }

  construct new(diagram) {
    _plots = deconstruct(diagram)
  }

  /* given a diagram "CCGG\nRRVV"
   * return a list of lists [["C","C","R","R"], ["G","G","V","V"]]
   */
  deconstruct(diagram) {
    var d = diagram.replace("\n", "")
    var c = d.count / 4
    return (0...c).map {|i| [ d[2*i], d[2*i + 1], d[2*c + 2*i], d[2*c + 2*i + 1] ]}.toList
  }

  plantsFor(student) {
    var i = __students.indexOf(student)
    if (i == -1) Fiber.abort("unknown student")
    return _plots[i].map {|c| __plants[c]}.toList
  }
}

KindergartenGarden.initialize_()
