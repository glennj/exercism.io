class EliudsEggs {
  static eggCount(number) {
    var count = 0
    while (number > 0) {
      count = count + (number & 1)
      number = number >> 1
    }
    return count
  }
}
