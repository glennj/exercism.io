import { describe, expect, test, test } from '@jest/globals';
import { discard, keep } from './strain';

describe('strain', () => {
  test('keeps on empty array returns empty array', () => {
    expect(keep([], (e) => e < 10)).toEqual([]);
  });

  test('keeps everything', () => {
    expect(keep([1, 2, 3], (e) => e < 10)).toEqual([1, 2, 3]);
  });

  test('keeps first and last', () => {
    expect(keep([1, 2, 3], (e) => e % 2 === 1)).toEqual([1, 3]);
  });

  test('keeps neither first nor last', () => {
    expect(keep([1, 2, 3, 4, 5], (e) => e % 2 === 0)).toEqual([2, 4]);
  });

  test('keeps strings', () => {
    const words = 'apple zebra banana zombies cherimoya zelot'.split(' ');
    const result = keep(words, (word) => word.indexOf('z') === 0);
    expect(result).toEqual('zebra zombies zelot'.split(' '));
  });

  test('keeps arrays', () => {
    const rows = [
      [1, 2, 3],
      [5, 5, 5],
      [5, 1, 2],
      [2, 1, 2],
      [1, 5, 2],
      [2, 2, 1],
      [1, 2, 5],
    ];
    const result = keep(rows, (row) => row.indexOf(5) > -1);
    expect(result).toEqual([
      [5, 5, 5],
      [5, 1, 2],
      [1, 5, 2],
      [1, 2, 5],
    ]);
  });

  test('empty discard', () => {
    expect(discard([], (e) => e < 10)).toEqual([]);
  });

  test('discards nothing', () => {
    expect(discard([1, 2, 3], (e) => e > 10)).toEqual([1, 2, 3]);
  });

  test('discards first and last', () => {
    expect(discard([1, 2, 3], (e) => e % 2 === 1)).toEqual([2]);
  });

  test('discards neither first nor last', () => {
    const result = discard([1, 2, 3, 4, 5], (e) => e % 2 === 0);
    expect(result).toEqual([1, 3, 5]);
  });

  test('discards strings', () => {
    const words = 'apple zebra banana zombies cherimoya zelot'.split(' ');
    const result = discard(words, (word) => word.indexOf('z') === 0);
    expect(result).toEqual('apple banana cherimoya'.split(' '));
  });

  test('discards arrays', () => {
    const rows = [
      [1, 2, 3],
      [5, 5, 5],
      [5, 1, 2],
      [2, 1, 2],
      [1, 5, 2],
      [2, 2, 1],
      [1, 2, 5],
    ];
    const result = discard(rows, (row) => row.indexOf(5) > -1);
    expect(result).toEqual([
      [1, 2, 3],
      [2, 1, 2],
      [2, 2, 1],
    ]);
  });
});
