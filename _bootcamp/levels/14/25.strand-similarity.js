export const hammingDistance = (strand1, strand2) =>
  [...strand1]
    .filter((char, idx) => char !== strand2[idx])
    .length;
