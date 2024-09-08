Array.prototype.last = () -> this[this.length - 1]
Array.prototype.take = (n) -> this.slice(0, n)

class HighScores
  constructor: (scores) -> @scores = scores.slice()
  latest: () -> @scores.last()
  personalBest: () -> Math.max(@scores...)
  personalTopThree: () -> @scores.slice().sort((a, b) -> b - a).take(3)

module.exports = HighScores
