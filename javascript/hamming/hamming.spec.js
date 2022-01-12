import { compute } from './hamming';

describe('Hamming', () => {
  test('empty strands', () => {
    expect(compute('', '')).toEqual(0);
  });

  test('single letter identical strands', () => {
    expect(compute('A', 'A')).toEqual(0);
  });

  test('single letter different strands', () => {
    expect(compute('G', 'T')).toEqual(1);
  });

  test('long identical strands', () => {
    expect(compute('GGACTGAAATCTG', 'GGACTGAAATCTG')).toEqual(0);
  });

  test('long different strands', () => {
    expect(compute('GGACGGATTCTG', 'AGGACGGATTCT')).toEqual(9);
  });

  test('disallow first strand longer', () => {
    expect(() => compute('AATG', 'AAA')).toThrow(
      new Error('strands must be of equal length')
    );
  });

  test('disallow second strand longer', () => {
    expect(() => compute('ATA', 'AGTG')).toThrow(
      new Error('strands must be of equal length')
    );
  });

  test('disallow empty first strand', () => {
    expect(() => compute('', 'G')).toThrow(
      new Error('strands must be of equal length')
    );
  });

  test('disallow empty second strand', () => {
    expect(() => compute('G', '')).toThrow(
      new Error('strands must be of equal length')
    );
  });
});
