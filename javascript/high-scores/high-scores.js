class HighScores {
  constructor(input) {
    this.scores = [];
    this.highest = -Infinity;
    this.top = [];
    input.forEach(score => this.addScore(score));
  }

  // add a new score, and update the high scores
  addScore(score) {
    this.scores.push(score);
    this.latest = score;
    this.top = this.top.concat(score).sort((a, b) => a - b).reverse().slice(0, 3);
    this.highest = this.top[0]; // eslint-disable-line prefer-destructuring
  }

  get report() {
    const diff = this.highest - this.latest;
    const shortOf = diff > 0 ? `${diff} short of ` : '';
    return `Your latest score was ${this.latest}. That's ${shortOf}your personal best!`;
  }
}

module.exports = { HighScores };
