class StringUtil {
  static reverse(string) {
    return string.reduce([]) {|reversed, char| 
      reversed.insert(0, char)
      return reversed
    }.join()
  }
}
