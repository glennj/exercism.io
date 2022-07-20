class Math {
  static floorMod(num, div) { (num % div + div) % div }
}

class Clock {
  construct new(hours, minutes) {
    _mins = hours * 60 + minutes
    normalize_
  }

  mins { _mins }

  normalize_ { _mins = Math.floorMod(_mins, 24 * 60) }

  toString { 
    var asTwoDigitString = Fn.new {|n| n < 10 ? "0%(n)" : "%(n)" }
    var h = asTwoDigitString.call((_mins / 60).floor)
    var m = asTwoDigitString.call(_mins % 60)
    return "%(h):%(m)"
  }

  add(minutes) {
    _mins = _mins + minutes
    normalize_
  }

  subtract(minutes) { add(-minutes) }

  ==(other) { other is type && mins == other.mins }
}
