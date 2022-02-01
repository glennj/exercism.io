import { Bucket } from './bucket';

const GCD = (a, b) => (b ? GCD(b, a % b) : a);

export class TwoBucket {
  constructor(vol1, vol2, goal, start) {
    if (goal > Math.max(vol1, vol2))
      throw new Error('No solution: goal bigger than largest bucket');

    const gcd = GCD(vol1, vol2);
    if (goal % gcd !== 0)
      throw new Error('No solution possible');

    this.goal = goal;

    if (start === 'one') {
      this.first  = new Bucket('one', vol1);
      this.second = new Bucket('two', vol2);
    }
    else {
      this.first  = new Bucket('two', vol2);
      this.second = new Bucket('one', vol1);
    }
  }

  solve() {
    this.first.empty();
    this.second.empty();
    let moves = 0;

    // fill the start bucket with the first move
    this.first.fill();
    moves += 1;

    // optimization: if the other bucket is the right
    // size, fill it immediately with thesecond move
    if (this.second.size === this.goal) {
      this.second.fill();
      moves += 1;
    }

    /* eslint-disable-next-line no-constant-condition */
    while (true) {
      if (this.first.amount === this.goal)
        return this.result(moves, this.first, this.second);

      if (this.second.amount === this.goal)
        return this.result(moves, this.second, this.first);

      if (this.first.isEmpty)
        this.first.fill();
      else if (this.second.isFull)
        this.second.empty();
      else
        this.first.pourInto(this.second);

      moves += 1;
    }
  }

  result(moves, winner, loser) {
    return {
      moves: moves,
      goalBucket: winner.name,
      otherBucket: loser.amount,
    }
  }
}
