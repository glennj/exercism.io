class SecretHandshake {
  static commands(code) {
    var actions = ["wink", "double blink", "close your eyes", "jump"]

    var acts = []
    (0...actions.count).each {|i| 
      if (bitAt(code, i) == 1) {
        acts.add(actions[i])
      }
    }
    return (bitAt(code, actions.count) == 1) ? reverse(acts) : acts
  }

  static bitAt(num, idx) { (num >> idx) & 1 }
  static reverse(list) { list.reduce([]) {|rev, e| [e] + rev}}
}
