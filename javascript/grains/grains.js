export const square = (square) => {
  if (square < 1 || square > 64) 
    throw new Error('square must be between 1 and 64');

  return BigInt(2)**BigInt(square - 1);
};

export const total = () => {
  return 2n ** 64n - 1n;
};
