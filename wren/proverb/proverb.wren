class Proverb {
  static recite(strings) {
    if (strings.count == 0) {
      return []
    }
    
    var line = Fn.new {|idx| "For want of a " + strings[idx - 1] + " the " + strings[idx] + " was lost." }
    return (1...strings.count).reduce([]) {|acc, idx| acc + [line.call(idx)] } +
           (strings.isEmpty ? [] : ["And all for the want of a " + strings[0] + "."])
  }
}
