const isPrime = (n) => {
  if (n == 2) return true;
  if (n % 2 === 0) return false;
  const j = Math.floor(Math.sqrt(n));
  for (let i = 3; i <= j; i += 2) if (n % i === 0) return false;
  return true;
};

export class DiffieHellman {
  constructor(p, g) {
    if (p < 2 || g < 2) throw new Error();
    if (!isPrime(p) || !isPrime(g)) throw new Error();
    this.p = p;
    this.g = g;
  }

  getPublicKey(n) {
    if (n < 2 || n >= this.p) throw new Error();
    return (this.g ** n) % this.p;
  }

  getSecret(yourPublicKey, myPrivateKey) {
    return (yourPublicKey ** myPrivateKey) % this.p;
  }

  // private key is greater than 1 and less than p
  static getPrivateKey = p => 2 + Math.floor(Math.random() * (p - 2));
}
