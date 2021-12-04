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
    other.amount += toPour;
  }
}

const gcd = (a, b) => b == 0 ? a : gcd(b, a % b);

class TwoBucket {
  constructor(vol1, vol2, goal, start) {
    this.goal = goal;
    if (start === 'one') {
      this.b1 = new Bucket('one', vol1);
      this.b2 = new Bucket('two', vol2);
    }
    else {
      this.b1 = new Bucket('two', vol2);
      this.b2 = new Bucket('one', vol1);
    }
  }

  moves() {
    const [vol1, vol2] = [this.b1.size, this.b2.size];

    if (this.goal > Math.max(vol1, vol2)) {
      throw new Error('No solution: goal bigger than largest bucket');
    }

    const GCD = gcd(vol1, vol2);
    if (GCD !== 1 && this.goal % GCD !== 0) {
      // buckets are not relatively prime. Goal must be a multiple of the GCD
      throw new Error('No solution possible');
    }

    return this.solve();
  }

  solve() {
    const start = this.b1;
    const other = this.b2;

    start.empty();
    other.empty();
    let moves = 0;

    // fill the start bucket with the first move
    start.fill();
    moves += 1;

    // optimization: if the other bucket is the right size,
    // fill it immediately with the second move
    if (other.size === this.goal) {
      other.fill();
      moves += 1;
    }

    while (true) {                    // eslint-disable-line no-constant-condition
      if (start.amount === this.goal) {
        [this.goalBucket, this.otherBucket] = [start.name, other.amount];
        return moves;
      }
      if (other.amount === this.goal) {
        [this.goalBucket, this.otherBucket] = [other.name, start.amount];
        return moves;
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
