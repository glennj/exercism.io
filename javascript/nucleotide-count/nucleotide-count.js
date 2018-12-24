/* eslint-disable  object-curly-newline, no-restricted-syntax, no-unused-vars */

const parse = (strand) => {
  if (/[^ACGT]/.test(strand)) {
    throw new Error('Invalid nucleotide in strand');
  }
  const count = { A: 0, C: 0, G: 0, T: 0 };
  for (const nucleotide of strand) {
    count[nucleotide] += 1;
  }
  return Object.entries(count).sort().map(([key, val]) => val).join(' ');
};

module.exports = { parse };
