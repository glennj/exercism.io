import { describe, expect, test, test } from '@jest/globals';
import { findAnagrams } from './anagram';

const areSetsEqual = (setA, setB) =>
  setA.size === setB.size && [...setA].every((val) => setB.has(val));

describe('Anagram', () => {
  test('no matches', () => {
    const expected = [];
    const actual = findAnagrams('diaper', [
      'hello',
      'world',
      'zombies',
      'pants',
    ]);
    expect(areSetsEqual(new Set(expected), new Set(actual))).toEqual(true);
  });

  test('detects two anagrams', () => {
    const expected = ['lemons', 'melons'];
    const actual = findAnagrams('solemn', ['lemons', 'cherry', 'melons']);
    expect(areSetsEqual(new Set(expected), new Set(actual))).toEqual(true);
  });

  test('does not detect anagram subsets', () => {
    const expected = [];
    const actual = findAnagrams('good', ['dog', 'goody']);
    expect(areSetsEqual(new Set(expected), new Set(actual))).toEqual(true);
  });

  test('detects anagram', () => {
    const expected = ['inlets'];
    const actual = findAnagrams('listen', [
      'enlists',
      'google',
      'inlets',
      'banana',
    ]);
    expect(areSetsEqual(new Set(expected), new Set(actual))).toEqual(true);
  });

  test('detects three anagrams', () => {
    const expected = ['gallery', 'regally', 'largely'];
    const actual = findAnagrams('allergy', [
      'gallery',
      'ballerina',
      'regally',
      'clergy',
      'largely',
      'leading',
    ]);
    expect(areSetsEqual(new Set(expected), new Set(actual))).toEqual(true);
  });

  test('detects multiple anagrams with different case', () => {
    const expected = ['Eons', 'ONES'];
    const actual = findAnagrams('nose', ['Eons', 'ONES']);
    expect(areSetsEqual(new Set(expected), new Set(actual))).toEqual(true);
  });

  test('does not detect non-anagrams with identical checksum', () => {
    const expected = [];
    const actual = findAnagrams('mass', ['last']);
    expect(areSetsEqual(new Set(expected), new Set(actual))).toEqual(true);
  });

  test('detects anagrams case-insensitively', () => {
    const expected = ['Carthorse'];
    const actual = findAnagrams('Orchestra', [
      'cashregister',
      'Carthorse',
      'radishes',
    ]);
    expect(areSetsEqual(new Set(expected), new Set(actual))).toEqual(true);
  });

  test('detects anagrams using case-insensitive subject', () => {
    const expected = ['carthorse'];
    const actual = findAnagrams('Orchestra', [
      'cashregister',
      'carthorse',
      'radishes',
    ]);
    expect(areSetsEqual(new Set(expected), new Set(actual))).toEqual(true);
  });

  test('detects anagrams using case-insensitive possible matches', () => {
    const expected = ['Carthorse'];
    const actual = findAnagrams('orchestra', [
      'cashregister',
      'Carthorse',
      'radishes',
    ]);
    expect(areSetsEqual(new Set(expected), new Set(actual))).toEqual(true);
  });

  test('does not detect an anagram if the original word is repeated', () => {
    const expected = [];
    const actual = findAnagrams('go', ['go Go GO']);
    expect(areSetsEqual(new Set(expected), new Set(actual))).toEqual(true);
  });

  test('anagrams must use all letters exactly once', () => {
    const expected = [];
    const actual = findAnagrams('tapper', ['patter']);
    expect(areSetsEqual(new Set(expected), new Set(actual))).toEqual(true);
  });

  test('words are not anagrams of themselves (case-insensitive)', () => {
    const expected = [];
    const actual = findAnagrams('BANANA', ['BANANA', 'Banana', 'banana']);
    expect(areSetsEqual(new Set(expected), new Set(actual))).toEqual(true);
  });

  test('words other than themselves can be anagrams', () => {
    const expected = ['Silent'];
    const actual = findAnagrams('LISTEN', ['Listen', 'Silent', 'LISTEN']);
    expect(areSetsEqual(new Set(expected), new Set(actual))).toEqual(true);
  });
});
