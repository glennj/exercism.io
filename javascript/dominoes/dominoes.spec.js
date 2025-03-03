import { describe, describe, expect, test, xtest } from '@jest/globals';
import { chain } from './dominoes';

function runTest(dominoes, expected) {
  if (expected) {
    runTestsExpectingChain(dominoes);
  } else {
    runTestsExpectingNull(dominoes);
  }
}

function runTestsExpectingNull(dominoes) {
  const result = chain(dominoes);

  test('Should not have a chain', () => {
    expect(result).toBe(null);
  });
}

function runTestsExpectingChain(dominoes) {
  const result = chain(dominoes);

  test('Should have a chain', () => {
    expect(result).not.toBe(null);
  });

  test('The number of dominoes in the output equals the number of dominoes in the input.', () => {
    expect(result).toHaveLength(dominoes.length);
  });

  test('For each adjacent pair of dominoes ... (a, b), (c, d) ...: b is equal to c.', () => {
    expect(
      result
        .map((v, i) => {
          if (i === result.length - 1) return true;
          return v[1] === result[i + 1][0];
        })
        .every(Boolean),
    ).toBe(true);
  });

  if (dominoes.length > 0) {
    test('For the dominoes on the ends (a, b) ... (c, d): a is equal to d.', () => {
      expect(result[0][0] === result[result.length - 1][1]).toBe(true);
    });
  }

  // 4. Every domino appears in the output an equal number of times as the number of times it appears in the input.
  // (in other words, the dominoes in the output are the same dominoes as the ones in the input)
  test('Should have the same dominoes', () => {
    const sortDomino = (domino) => [...domino].sort();
    expect([...dominoes].map(sortDomino).sort()).toEqual(
      [...result].map(sortDomino).sort(),
    );
  });
}

describe('Dominoes', () => {
  describe('empty input = empty output', () => {
    runTest([], true);
  });

  describe('singleton input = singleton output', () => {
    runTest([[1, 1]], true);
  });

  describe("singleton that can't be chained", () => {
    runTest([[1, 2]], false);
  });

  describe('three elements', () => {
    runTest(
      [
        [1, 2],
        [3, 1],
        [2, 3],
      ],
      true,
    );
  });

  describe('can reverse dominoes', () => {
    runTest(
      [
        [1, 2],
        [1, 3],
        [2, 3],
      ],
      true,
    );
  });

  describe("can't be chained", () => {
    runTest(
      [
        [1, 2],
        [4, 1],
        [2, 3],
      ],
      false,
    );
  });

  describe('disconnected - simple', () => {
    runTest(
      [
        [1, 1],
        [2, 2],
      ],
      false,
    );
  });

  describe('disconnected - double loop', () => {
    runTest(
      [
        [1, 2],
        [2, 1],
        [3, 4],
        [4, 3],
      ],
      false,
    );
  });

  describe('disconnected - single isolated', () => {
    runTest(
      [
        [1, 2],
        [2, 3],
        [3, 1],
        [4, 4],
      ],
      false,
    );
  });

  describe('need backtrack', () => {
    runTest(
      [
        [1, 2],
        [2, 3],
        [3, 1],
        [2, 4],
        [2, 4],
      ],
      true,
    );
  });

  describe('separate loops', () => {
    runTest(
      [
        [1, 2],
        [2, 3],
        [3, 1],
        [1, 1],
        [2, 2],
        [3, 3],
      ],
      true,
    );
  });

  describe('nine elements', () => {
    runTest(
      [
        [1, 2],
        [5, 3],
        [3, 1],
        [1, 2],
        [2, 4],
        [1, 6],
        [2, 3],
        [3, 4],
        [5, 6],
      ],
      true,
    );
  });

  describe('separate three-domino loops', () => {
    runTest(
      [
        [1, 2],
        [2, 3],
        [3, 1],
        [4, 5],
        [5, 6],
        [6, 4],
      ],
      false,
    );
  });
});
