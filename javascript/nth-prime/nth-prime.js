/* eslint brace-style: ["error", "stroustrup", { "allowSingleLine": true }] */

const isPrime = (n) => {
  if (n < 2) return false;
  if (n === 2) return true;
  if (n % 2 === 0) return false;
  for (let i = 3; i * i <= n; i += 2) {
    if (n % i === 0) return false;
  }
  return true;
};

/*
// Iterator protocol
function PrimeNumberIterator() {
  let p;

  return {
    next: () => {
      if (p === undefined) {
        p = 2;
      }
      else if (p === 2) {
        p = 3;
      }
      else {
        do { p += 2; } while (!isPrime(p));
      }
      return { value: p, done: false };
    },
  };
}
*/

// Iterable protocol
function* PrimeNumberGenerator() {
  yield 2;
  yield 3;
  let p = 3;
  while (true) {
    do { p += 2; } while (!isPrime(p));
    yield p;
  }
}

class Prime {
  constructor() {
    this.primes = [];
    // this.png = PrimeNumberIterator();
    this.png = PrimeNumberGenerator();
  }

  nth(n) {
    if (n === 0) throw new Error('Prime is not possible');
    while (this.primes.length < n) {
      const p = this.png.next().value;
      this.primes.push(p);
    }
    return this.primes[n - 1];
  }
}

module.exports = Prime;
