/* Two ways to use this class:
 *     const s = new Sieve(100); console.log(s.primes);
 *     const primes = Sieve.eratosthenes(100); console.log(primes);
 */

class Sieve {
  constructor(limit) {
    if (!Number.isInteger(limit) || limit < 2) {
      throw new Error('invalid argument');
    }
    this.primes = Sieve.eratosthenes(limit);
  }

  static eratosthenes(limit) {
    const candidates = new Array(limit + 1).fill(true);
    candidates[0] = false;
    candidates[1] = false;

    const removeMultiples = (m) => {
      const step = m === 2 ? 2 : m * 2;
      for (let i = m * m; i <= limit; i += step) {
        candidates[i] = false;
      }
    };

    // remove multiples of 2
    removeMultiples(2);

    // remove multiples of 3, 5, 7, ...
    for (let n = 3; n * n <= limit; n += 2) {
      removeMultiples(n);
    }

    // indices of true elements are prime
    return candidates
      .map((v, i) => [i, v])
      .filter(([i, v]) => v)
      .map(([i, v]) => i);
  }
}

export const primes = (limit) => Sieve.eratosthenes(limit);
