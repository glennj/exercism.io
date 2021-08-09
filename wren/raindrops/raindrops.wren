class Raindrops {
  static convert(n) {
    var drops = ""
    if (n % 3 == 0) drops = drops + "Pling"
    if (n % 5 == 0) drops = drops + "Plang"
    if (n % 7 == 0) drops = drops + "Plong"
    return drops.isEmpty ? n.toString : drops
  }
}
