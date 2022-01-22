export class HighScores {
  #scores;

  constructor(input) {
    this.#scores = input.slice();
  }

  // returns a _copy_ of #scores, sorted in descending numerical order
  #sorted() {
    return this.#scores.slice().sort((a, b) => b - a);
  }

  // return a _copy_ so the consumer can't modify my scores
  get scores() {
    return this.#scores.slice();
  }

  get latest() {
    //return this.#scores.at(-1);
    const s = this.#scores;
    return s[s.length - 1];
  }

  get personalBest() {
    //return this.#sorted().at(0);
    return this.#sorted()[0];
  }

  get personalTopThree() {
    return this.#sorted().slice(0, 3);
  }
}
