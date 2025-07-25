import { describe, expect, test } from '@jest/globals';
import { annotate } from './flower-field';

describe('Flower Field', () => {
  test('handles no rows', () => {
    expect(annotate([])).toEqual([]);
  });

  test('handles no columns', () => {
    expect(annotate([''])).toEqual(['']);
  });

  test('handles no flowers', () => {
    const input = ['   ', '   ', '   '];
    const expected = ['   ', '   ', '   '];
    expect(annotate(input)).toEqual(expected);
  });

  test('handles garden full of flowers', () => {
    const input = ['***', '***', '***'];
    const expected = ['***', '***', '***'];
    expect(annotate(input)).toEqual(expected);
  });

  test('handles flower surrounded by spaces', () => {
    const input = ['   ', ' * ', '   '];
    const expected = ['111', '1*1', '111'];
    expect(annotate(input)).toEqual(expected);
  });

  test('handles space surrounded by flowers', () => {
    const input = ['***', '* *', '***'];
    const expected = ['***', '*8*', '***'];
    expect(annotate(input)).toEqual(expected);
  });

  test('handles horizontal line', () => {
    const input = [' * * '];
    const expected = ['1*2*1'];
    expect(annotate(input)).toEqual(expected);
  });

  test('handles horizontal line, flowers at edges', () => {
    const input = ['*   *'];
    const expected = ['*1 1*'];
    expect(annotate(input)).toEqual(expected);
  });

  test('handles vertical line', () => {
    const input = [' ', '*', ' ', '*', ' '];
    const expected = ['1', '*', '2', '*', '1'];
    expect(annotate(input)).toEqual(expected);
  });

  test('handles vertical line, flowers at edges', () => {
    const input = ['*', ' ', ' ', ' ', '*'];
    const expected = ['*', '1', ' ', '1', '*'];
    expect(annotate(input)).toEqual(expected);
  });

  test('handles cross', () => {
    const input = ['  *  ', '  *  ', '*****', '  *  ', '  *  '];
    const expected = [' 2*2 ', '25*52', '*****', '25*52', ' 2*2 '];
    expect(annotate(input)).toEqual(expected);
  });

  test('handles large garden', () => {
    const input = [' *  * ', '  *   ', '    * ', '   * *', ' *  * ', '      '];
    const expected = [
      '1*22*1',
      '12*322',
      ' 123*2',
      '112*4*',
      '1*22*2',
      '111111',
    ];
    expect(annotate(input)).toEqual(expected);
  });
});
