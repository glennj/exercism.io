import { describe, expect, test } from '@jest/globals';
import { DiffieHellman } from './diffie-hellman';

describe('diffie-hellman', () => {
  test('throws an error if the constructor arguments are out of range', () => {
    expect(() => {
      new DiffieHellman(0, 9999);
    }).toThrow();
  });

  test('throws an error if the constructor arguments are not prime', () => {
    expect(() => {
      new DiffieHellman(10, 13);
    }).toThrow();
  });

  describe('private key is greater than 1 and less than p', () => {
    const p = 23;
    const g = 5;
    const diffieHellman = new DiffieHellman(p, g);

    test('throws an error if private key is negative', () => {
      expect(() => {
        diffieHellman.getPublicKey(-1);
      }).toThrow();
    });

    test('throws an error if private key is zero', () => {
      expect(() => {
        diffieHellman.getPublicKey(0);
      }).toThrow();
    });

    test('throws an error if private key is one', () => {
      expect(() => {
        diffieHellman.getPublicKey(1);
      }).toThrow();
    });

    test('throws an error if private key equals the modulus parameter p', () => {
      expect(() => {
        diffieHellman.getPublicKey(p);
      }).toThrow();
    });

    test('throws an error if private key is greater than the modulus parameter p', () => {
      expect(() => {
        diffieHellman.getPublicKey(p + 1);
      }).toThrow();
    });
  });

  describe('stateless calculation', () => {
    const diffieHellman = new DiffieHellman(23, 5);

    const alicePrivateKey = 6;
    const alicePublicKey = 8;

    const bobPrivateKey = 15;
    const bobPublicKey = 19;

    test('can calculate public key using private key', () => {
      expect(diffieHellman.getPublicKey(alicePrivateKey)).toEqual(
        alicePublicKey,
      );
    });

    test('can calculate public key when given a different private key', () => {
      expect(diffieHellman.getPublicKey(bobPrivateKey)).toEqual(bobPublicKey);
    });
  });

  test("can calculate secret using other party's public key", () => {
    expect(new DiffieHellman(23, 5).getSecret(19, 6)).toEqual(2);
  });

  test('key exchange', () => {
    const diffieHellman = new DiffieHellman(23, 5);

    const alicePrivateKey = 6;
    const bobPrivateKey = 15;
    const alicePublicKey = diffieHellman.getPublicKey(alicePrivateKey);
    const bobPublicKey = diffieHellman.getPublicKey(bobPrivateKey);

    const secretA = diffieHellman.getSecret(bobPublicKey, alicePrivateKey);
    const secretB = diffieHellman.getSecret(alicePublicKey, bobPrivateKey);

    expect(secretA).toEqual(secretB);
  });

  test('private key is greater than 1 and less than p', () => {
    let p = 23;
    for (let i = 0; i < 10; i++) {
      let privateKey = DiffieHellman.getPrivateKey(p);
      expect(privateKey).toBeGreaterThan(1);
      expect(privateKey).toBeLessThan(p);
    }
  });

  test('private key is random', () => {
    let p = 7919;
    let uniqueKeys = new Set();
    let testIterations = 1000;

    for (let i = 0; i < testIterations; i++) {
      uniqueKeys.add(DiffieHellman.getPrivateKey(p));
    }

    expect(uniqueKeys.size).toBeGreaterThan(testIterations - 100);
  });
});
