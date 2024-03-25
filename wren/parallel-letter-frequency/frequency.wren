class Frequency {
  static ofText(text) {
    return text.codePoints.reduce(Frequency.new()) {|freq, cp|
      var char
      if (65 <= cp && cp <= 90) char = String.fromCodePoint(cp + 32)
      if (97 <= cp && cp <= 122) char = String.fromCodePoint(cp)
      if (char != null) freq.incr(char)
      return freq
    }
  }

  construct new() {
    _map = {}
  }

  keys { _map.keys }
  [key] { _map[key] }
  [key]=(value) { _map[key] = value }

  toMap { _map }
  toString { _map.toString }

  incr(key) { incr(key, 1) }
  incr(key, amount) { _map[key] = (_map.containsKey(key) ? _map[key] : 0) + amount }

  addAll(other) {
    if (other is Frequency) {
      other.keys.each {|key|
        incr(key, other[key])
      }
    }
  }
}
