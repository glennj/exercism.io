/* Using the [0-1 knapsack problem][1] as specified on wikipedia
 * [1]: https://en.wikipedia.org/wiki/Knapsack_problem#0-1_knapsack_problem
 */

class Knapsack {
  static maximumValue(maximumWeight, items) {
    if (items.count == 0) return 0
    var m = (0..items.count).map {|i| List.filled(maximumWeight + 1, 0)}.toList
    for (i in 0...items.count) {
      for (j in 1..maximumWeight) {
        if (items[i]["weight"] > j) {
          m[i+1][j] = m[i][j]
        } else {
          m[i+1][j] = m[i][j].max( m[i][j - items[i]["weight"]] + items[i]["value"] )
        }
      }
    }
    return m[items.count][maximumWeight]
  }
}
