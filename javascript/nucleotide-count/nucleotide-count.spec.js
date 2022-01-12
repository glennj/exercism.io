import { countNucleotides } from './nucleotide-count';

describe('count all nucleotides in a strand', () => {
  test('empty strand', () => {
    expect(countNucleotides('')).toEqual('0 0 0 0');
  });

  test('can count one nucleotide in single-character input', () => {
    expect(countNucleotides('G')).toEqual('0 0 1 0');
  });

  test('strand with repeated nucleotide', () => {
    expect(countNucleotides('GGGGGGG')).toEqual('0 0 7 0');
  });

  test('strand with multiple nucleotides', () => {
    expect(
      countNucleotides(
        'AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC'
      )
    ).toEqual('20 12 17 21');
  });

  test('strand with invalid nucleotides', () => {
    expect(() => countNucleotides('AGXXACT')).toThrow(
      new Error('Invalid nucleotide in strand')
    );
  });
});
