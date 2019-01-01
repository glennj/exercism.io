import { secretHandshake } from './secret-handshake';

describe('Secret Handshake', () => {
  test('binary 1 (decimal 1) is a wink', () => {
    expect(secretHandshake(1)).toEqual(['wink']);
  });

  test('binary 10 (decimal 2) is a double blink', () => {
    expect(secretHandshake(2)).toEqual(['double blink']);
  });

  test('binary 100 (decimal 4) is close your eyes', () => {
    expect(secretHandshake(4)).toEqual(['close your eyes']);
  });

  test('binary 1000 (decimal 8) is jump', () => {
    expect(secretHandshake(8)).toEqual(['jump']);
  });

  test('binary 11 (decimal 3) is wink and double blink', () => {
    expect(secretHandshake(3)).toEqual(['wink', 'double blink']);
  });

  test('binary 100001 (decimal 33) is wink', () => {
    expect(secretHandshake(33)).toEqual(['wink']);
  });

  test('binary 10011 (decimal 19) is double blink and wink', () => {
    expect(secretHandshake(19)).toEqual(['double blink', 'wink']);
  });

  test('binary 1010011 (decimal 83) is double blink and wink', () => {
    expect(secretHandshake(83)).toEqual(['double blink', 'wink']);
  });

  test('binary 11111 (decimal 31) is jump, close your eyes, double blink, and wink', () => {
    expect(secretHandshake(31)).toEqual(['jump', 'close your eyes', 'double blink', 'wink']);
  });

  test('text is an invalid secret handshake', () => {
    expect(() => secretHandshake('piggies'))
      .toThrow(new Error('Handshake must be a number'));
  });
});
