import { describe, expect, test, test } from '@jest/globals';
import { eggCount } from './eliuds-eggs';

describe('EliudsEggs', () => {
  test('0 eggs', () => {
    const expected = 0;
    const actual = eggCount(0);
    expect(actual).toEqual(expected);
  });

  test('1 egg', () => {
    const expected = 1;
    const actual = eggCount(16);
    expect(actual).toEqual(expected);
  });

  test('4 eggs', () => {
    const expected = 4;
    const actual = eggCount(89);
    expect(actual).toEqual(expected);
  });

  test('13 eggs', () => {
    const expected = 13;
    const actual = eggCount(2000000000);
    expect(actual).toEqual(expected);
  });
});
