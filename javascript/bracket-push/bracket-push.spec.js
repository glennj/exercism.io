import { bracketPush } from './bracket-push';

describe('bracket push', () => {
  test('checks for appropriate bracketing in a set of brackets', () => {
    expect(bracketPush('{}')).toEqual(true);
  });

  test('returns false for unclosed brackets', () => {
    expect(bracketPush('{{')).toEqual(false);
  });

  test('returns false if brackets are out of order', () => {
    expect(bracketPush('}{')).toEqual(false);
  });

  test('checks bracketing in more than one pair of brackets', () => {
    expect(bracketPush('{}[]')).toEqual(true);
  });

  test('checks bracketing in nested brackets', () => {
    expect(bracketPush('{[]}')).toEqual(true);
  });

  test('rejects brackets that are properly balanced but improperly nested', () => {
    expect(bracketPush('{[}]')).toEqual(false);
  });

  test('checks bracket closure with deeper nesting', () => {
    expect(bracketPush('{[)][]}')).toEqual(false);
  });

  test('checks bracket closure in a long string of brackets', () => {
    expect(bracketPush('{[]([()])}')).toEqual(true);
  });
});
