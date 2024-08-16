BigInt square(final int n, [int size = 64]) {
  if (n < 1 || n > size) {
    throw ArgumentError('square must be between 1 and $size');
  }
  return BigInt.two.pow(n - 1);
}

BigInt total([int size = 64]) => BigInt.two.pow(size) - BigInt.one;
