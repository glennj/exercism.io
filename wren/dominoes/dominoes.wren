class Dominoes {
  static canChain(dominoes) {
    if (dominoes.isEmpty) return true

    for (i in 0...dominoes.count) {
      var d = dominoes[i]
      var rest = removeIdx(dominoes, i)
      var chain = chainFrom([d], rest)
      if (chain == null) chain = chainFrom([reverse(d)], rest)
      if (chain != null) return true
    }
    return false
  }

  static chainFrom(chain, remaining) {
    var tail = chain[-1][1]
    if (remaining.isEmpty) return chain[0][0] == tail ? chain : null

    for (i in 0...remaining.count) {
      var d = remaining[i]
      if (d.contains(tail)) {
        if (d[0] != tail) d = reverse(d)
        var rest = removeIdx(remaining, i)
        var newChain = chainFrom(chain + [d], rest)
        if (newChain != null) return newChain
      }
    }
    return null
  }

  // a List method: remove an index, non-mutating
  static removeIdx(list, idx) {
    var clone = list[0..-1]
    clone.removeAt(idx)
    return clone
  }

  static reverse(d) { d[1..0] }
}
