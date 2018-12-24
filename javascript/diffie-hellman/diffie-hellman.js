const isPrime = (n) => {
  if (n % 2 === 0) return false;
  const j = Math.floor(Math.sqrt(n));
  for (let i = 3; i <= j; i += 2) {
    if (n % i === 0) return false;
  }
  return true;
};

export default class DiffieHellman {
  constructor(p, g) {
    if (p < 2 || g < 2) throw new Error();
    if (!isPrime(p) || !isPrime(g)) throw new Error();
    this.p = p;
    this.g = g;
  }

  getPublicKeyFromPrivateKey(n) {
    if (n < 2 || n >= this.p) throw new Error();
    return (this.g ** n) % this.p;
  }

  getSharedSecret(a, b) {
    return (b ** a) % this.p;
  }
}
