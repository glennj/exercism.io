class StringUtils {
  static sort(list) { list.sort {|a, b| le(a, b)} }

  // `lt`, `le`, `gt`, `ge` -- borrowed from Perl
  static lt(a, b) {
    var cp1 = a.codePoints
    var cp2 = b.codePoints
    var c1 = cp1.count
    var c2 = cp2.count
    var minLen = c1 < c2 ? c1 : c2

    for (i in 0...minLen) {
      if (cp1[i] != cp2[i]) return cp1[i] < cp2[i]
    }
    return c1 < c2
  }

  static le(a, b) { a == b || lt(a, b) }
  static gt(a, b) { !le(a, b) }
  static ge(a, b) { !lt(a, b) }
}
