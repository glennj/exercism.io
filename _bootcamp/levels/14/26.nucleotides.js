export function countNucleotides(dna) {
  const count = {A:0, C:0, G:0, T:0};
  for (const char of dna) {
    if (!(char in count)) return false;
    count[char] += 1;
  }
  return count;
}
