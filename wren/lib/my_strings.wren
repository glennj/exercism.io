import "./zip" for Zip

class MyStrings {
  static lessThan(a, b) {
    var zipped = Zip.new(a.codePoints, b.codePoints)
    for (pair in zipped) {
      if (pair[0] != pair[1]) {
        return pair[0] < pair[1]
      }
    }
    return a.count < b.count
  }

  static sort(listOfStrings) {
    return listOfStrings.sort {|a, b| lessThan(a, b)}
  }
}
