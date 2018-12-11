import { compute } from './hamming';

describe('Hamming', () => {
  test('no difference between empty strands', () => {
    expect(compute('', '')).toEqual(0);
  });

  test('no difference between identical strands', () => {
    expect(compute('A', 'A')).toEqual(0);
  });

  test('long identical strands', () => {
    expect(compute('GGACTGA', 'GGACTGA')).toEqual(0);
  });

  test('complete distance in single nucleotide strands', () => {
    expect(compute('A', 'G')).toEqual(1);
  });

  test('complete distance in small strands', () => {
    expect(compute('AG', 'CT')).toEqual(2);
  });

  test('small distance in small strands', () => {
    expect(compute('AT', 'CT')).toEqual(1);
  });

  test('small distance', () => {
    expect(compute('GGACG', 'GGTCG')).toEqual(1);
  });

  test('small distance in long strands', () => {
    expect(compute('ACCAGGG', 'ACTATGG')).toEqual(2);
  });

  test('non-unique character in first strand', () => {
    expect(compute('AAG', 'AAA')).toEqual(1);
  });

  test('non-unique character in second strand', () => {
    expect(compute('AAA', 'AAG')).toEqual(1);
  });

  test('same nucleotides in different positions', () => {
    expect(compute('TAG', 'GAT')).toEqual(2);
  });

  test('large distance', () => {
    expect(compute('GATACA', 'GCATAA')).toEqual(4);
  });

  test('large distance in off-by-one strand', () => {
    expect(compute('GGACGGATTCTG', 'AGGACGGATTCT')).toEqual(9);
  });

  test('disallow first strand longer', () => {
    expect(() => compute('AATG', 'AAA')).toThrow(
      new Error('left and right strands must be of equal length'),
    );
  });

  test('disallow second strand longer', () => {
    expect(() => compute('ATA', 'AGTG')).toThrow(
      new Error('left and right strands must be of equal length'),
    );
  });
});
