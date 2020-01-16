/* eslint-disable  lines-between-class-members, no-multi-spaces */
/* eslint brace-style: ["error", "stroustrup", { "allowSingleLine": true }] */
/* eslint no-underscore-dangle: ["error", { "allowAfterThis": true }] */

class Bucket {
  constructor(name, size) {
    this.name = name;
    this.size = size;
    this.amount = 0;
  }

  get capacity() { return this.size - this.amount; }

  fill() { this.amount = this.size; }
  empty() { this.amount = 0; }
  isFull() { return this.amount === this.size; }
  isEmpty() { return this.amount === 0; }

  pour(other) {
    const toPour = Math.min(this.amount, other.capacity);
    this.amount -= toPour;
    other.amount += toPour;     // eslint-disable-line no-param-reassign
  }
}

const GCD = (a, b) => (b ? GCD(b, a % b) : a);

class TwoBucket {
  constructor(vol1, vol2, goal, start) {
    if (goal > Math.max(vol1, vol2)) {
      throw new Error('No solution: goal bigger than largest bucket');
    }

    const gcd = GCD(vol1, vol2);
    if (gcd !== 1 && goal % gcd !== 0) {
      // buckets are not relatively prime. Goal must be a multiple of the gcd
      throw new Error('No solution possible');
    }

    let b1;
    let b2;
    if (start === 'one') {
      b1 = new Bucket('one', vol1);
      b2 = new Bucket('two', vol2);
    }
    else {
      b1 = new Bucket('two', vol2);
      b2 = new Bucket('one', vol1);
    }

    this.solve(b1, b2, goal);
  }

  moves() { return this._moves; }

  solve(start, other, goal) {
    start.empty();
    other.empty();
    let moves = 0;

    // fill the start bucket with the first move
    start.fill();
    moves += 1;

    // optimization: if the other bucket is the right
    // size, fill it immediately with the second move
    if (other.size === goal) {
      other.fill();
      moves += 1;
    }

    while (true) {                    // eslint-disable-line no-constant-condition
      if (start.amount === goal) {
        [this.goalBucket, this.otherBucket, this._moves] = [start.name, other.amount, moves];
        return;
      }
      if (other.amount === goal) {
        [this.goalBucket, this.otherBucket, this._moves] = [other.name, start.amount, moves];
        return;
      }

      if (start.isEmpty()) {
        start.fill();
      }
      else if (other.isFull()) {
        other.empty();
      }
      else {
        start.pour(other);
      }
      moves += 1;
    }
  }
}

module.exports = { TwoBucket };
