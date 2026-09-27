int SIZE = 64;

BigInt square(final int n) {
  if (n < 1 || n > SIZE) {
    throw ArgumentError('square must be between 1 and $SIZE');
  }
  return BigInt.two.pow(n - 1);
}
 
BigInt total() => BigInt.two.pow(SIZE) - BigInt.one;
