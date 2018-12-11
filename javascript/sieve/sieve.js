/* Two ways to use this class:
 *     const s = new Sieve(100); console.log(s.primes);
 *     const primes = Sieve.eratosthenes(100); console.log(primes);
 */

class Sieve {
  constructor(limit) {
    if (!Number.isInteger(limit) || limit < 2) throw new Error('invalid argument');
    this.primes = Sieve.eratosthenes(limit);
  }

  static eratosthenes(limit) {
    const list = new Array(limit + 1).fill(1);

    // remove multiples of 2
    for (let m = 2 * 2; m <= limit; m += 2) {
      list[m] = null;
    }

    // remove multiples of 3, 5, 7, ...
    // but we can skip 9, 25, 49, ...
    const sqrt = Math.floor(Math.sqrt(limit));
    for (let n = 3; n <= sqrt; n += 2) {
      if (!Number.isInteger(Math.sqrt(n))) {
        for (let m = n * n; m <= limit; m += 2 * n) {
          list[m] = null;
        }
      }
    }

    // indices of non-null elements are prime, except 0 and 1.
    return list
      .map((v, i) => ({ i, v }))
      .filter(({ i, v }) => i >= 2 && v !== null)
      .map(({ i }) => i);
  }
}

module.exports = Sieve;


/* community
 *
 * clever solution: use 0 not null and smarter map+filter

    module.exports = function(limit) {
      // init
      let nums = Array(limit + 1).fill(1);
      nums[0]  = 0;
      nums[1]  = 0;

      let sieve = 2;
      while(sieve <= limit) {
        // mark the composites
        for(let compo = sieve * sieve; compo <= limit; compo += sieve) {
          nums[compo] = 0;
        }
        // next sieve
        do {
          sieve += 1;
        } while(sieve <= limit && !nums[sieve]);
      }

      this.primes = nums.map((v, k) => v * k).filter(v => !!v);
    };

 *
 * using generators

    function* filter(seq, prime) {
      for (let num of seq) {
        if (num % prime !== 0) {
          yield num;
        }
      }
    }

    function* numbers(start) {
      while (true) {
        yield start++;
      }
    }

    function* primes() {
      var seq = numbers(2);
      var prime;

      while (true) {
        prime = seq.next().value;
        yield prime;
        seq = filter(seq, prime);
      }
    }

    function* take(limit, seq) {
      let current;
      yield current = seq.next().value;
      while (current < limit) {
        yield current = seq.next().value;
      }
    }

    function Sieve(limit) {
      const collectedPrimes = [...take(limit, primes())].filter((num) => num <= limit);

      return Object.freeze({
        primes: collectedPrimes
      });
    }

    export default Sieve;

 */
