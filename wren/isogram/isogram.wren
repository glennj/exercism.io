import "essentials" for Strings

class Isogram {
  static isIsogram(string) {
    var result = true 
    var seen = {}
    for (c in Strings.upcase(string)) {
      if (!"ABCDEFGHIJKLMNOPQRSTUVWXYZ".contains(c)) continue
      if (seen.containsKey(c)) {
        result = false
        break
      }
      seen[c] = true
    }
    return result
  }
}
