class ArgumentError extends Error {}

export function square(s: number): bigint {
  if (s < 1 || s > 64)
    throw new ArgumentError('Invalid square')

  return 2n ** BigInt(s - 1)
}

export function total(): bigint {
  return 2n ** 64n - 1n
}
