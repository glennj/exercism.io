import Anagram from './anagram';

describe('Anagram', () => {
  test('no matches', () => {
    const subject = new Anagram('diaper');
    const matches = subject.matches(['hello', 'world', 'zombies', 'pants']);

    expect(matches).toEqual([]);
  });

  test('detects two anagrams', () => {
    const subject = new Anagram('master');
    const matches = subject.matches(['stream', 'pigeon', 'maters']);

    expect(matches).toEqual(['stream', 'maters']);
  });

  test('does not detect anagram subsets', () => {
    const subject = new Anagram('good');
    const matches = subject.matches(['dog', 'goody']);

    expect(matches).toEqual([]);
  });

  test('detects anagram', () => {
    const subject = new Anagram('listen');
    const matches = subject.matches(['enlists', 'google', 'inlets', 'banana']);

    expect(matches).toEqual(['inlets']);
  });

  test('detects three anagrams', () => {
    const subject = new Anagram('allergy');
    const matches = subject.matches(['gallery', 'ballerina', 'regally', 'clergy', 'largely', 'leading']);

    expect(matches).toEqual(['gallery', 'regally', 'largely']);
  });

  test('does not detect non-anagrams with identical checksum', () => {
    const subject = new Anagram('mass');
    const matches = subject.matches(['last']);

    expect(matches).toEqual([]);
  });

  test('detects anagrams case-insensitively', () => {
    const subject = new Anagram('Orchestra');
    const matches = subject.matches(['cashregister', 'Carthorse', 'radishes']);

    expect(matches).toEqual(['Carthorse']);
  });

  test('detects anagrams using case-insensitive subject', () => {
    const subject = new Anagram('Orchestra');
    const matches = subject.matches(['cashregister', 'carthorse', 'radishes']);

    expect(matches).toEqual(['carthorse']);
  });

  test('detects anagrams using case-insensitive possible matches', () => {
    const subject = new Anagram('orchestra');
    const matches = subject.matches(['cashregister', 'Carthorse', 'radishes']);

    expect(matches).toEqual(['Carthorse']);
  });

  test('does not detect a anagram if the original word is repeated', () => {
    const subject = new Anagram('go');
    const matches = subject.matches(['go Go GO']);

    expect(matches).toEqual([]);
  });

  test('anagrams must use all letters exactly once', () => {
    const subject = new Anagram('tapper');
    const matches = subject.matches(['patter']);

    expect(matches).toEqual([]);
  });

  test('capital word is not own anagram', () => {
    const subject = new Anagram('BANANA');
    const matches = subject.matches(['Banana']);

    expect(matches).toEqual([]);
  });
});
