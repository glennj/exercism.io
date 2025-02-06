import { describe, expect, test, test } from '@jest/globals';
import { truncate } from './micro-blog';

describe('Micro-blog', () => {
  test('English language short', () => {
    const inputString = 'Hi';
    const expected = 'Hi';
    const actual = truncate(inputString);
    expect(actual).toEqual(expected);
  });

  test('English language long', () => {
    const inputString = 'Hello there';
    const expected = 'Hello';
    const actual = truncate(inputString);
    expect(actual).toEqual(expected);
  });

  test('German language short (broth)', () => {
    const inputString = 'brühe';
    const expected = 'brühe';
    const actual = truncate(inputString);
    expect(actual).toEqual(expected);
  });

  test('German language long (bear carpet → beards)', () => {
    const inputString = 'Bärteppich';
    const expected = 'Bärte';
    const actual = truncate(inputString);
    expect(actual).toEqual(expected);
  });

  test('Bulgarian language short (good)', () => {
    const inputString = 'Добър';
    const expected = 'Добър';
    const actual = truncate(inputString);
    expect(actual).toEqual(expected);
  });

  test('Greek language short (health)', () => {
    const inputString = 'υγειά';
    const expected = 'υγειά';
    const actual = truncate(inputString);
    expect(actual).toEqual(expected);
  });

  test('Maths short', () => {
    const inputString = 'a=πr²';
    const expected = 'a=πr²';
    const actual = truncate(inputString);
    expect(actual).toEqual(expected);
  });

  test('Maths long', () => {
    const inputString = '∅⊊ℕ⊊ℤ⊊ℚ⊊ℝ⊊ℂ';
    const expected = '∅⊊ℕ⊊ℤ';
    const actual = truncate(inputString);
    expect(actual).toEqual(expected);
  });

  test('English and emoji short', () => {
    const inputString = 'Fly 🛫';
    const expected = 'Fly 🛫';
    const actual = truncate(inputString);
    expect(actual).toEqual(expected);
  });

  test('Emoji short', () => {
    const inputString = '💇';
    const expected = '💇';
    const actual = truncate(inputString);
    expect(actual).toEqual(expected);
  });

  test('Emoji long', () => {
    const inputString = '❄🌡🤧🤒🏥🕰😀';
    const expected = '❄🌡🤧🤒🏥';
    const actual = truncate(inputString);
    expect(actual).toEqual(expected);
  });

  test('Royal Flush?', () => {
    const inputString = '🃎🂸🃅🃋🃍🃁🃊';
    const expected = '🃎🂸🃅🃋🃍';
    const actual = truncate(inputString);
    expect(actual).toEqual(expected);
  });
});
