// Using the Binary numeral system (base 2) from Wikipedia
// https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_%28base_2%29
//

export const squareRoot = (n) => {
  // find `b`, then greatest power of 4 <= n
  let b = Math.pow(4, Math.floor(Math.log(n) / Math.log(4)));
  let x = 0;

  while (b != 0) {
    if (n >= x + b) {
      n = n - x - b;
      x = (x >> 1) + b;
    }
    else {
      x >>= 1;
    }
    b >>= 2;
  }
  return(x);
};
