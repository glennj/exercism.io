const isPrime = (num: number): boolean => {
  if (num % 2 === 0) { return false }
  const limit = Math.floor(Math.sqrt(num))
  for (let i = 3; i <= limit; i += 2) {
    if (num % i === 0) { return false }
  }
  return true
}

class DiffieHellman {
  private readonly p: number
  private readonly g: number

  constructor(p: number, g: number) {
    if (p < 2 || g < 2) {
      throw new Error('out of range')
    }
    if (!isPrime(p) || !isPrime(g)) {
      throw new Error('not prime')
    }
    this.p = p
    this.g = g
  }

  getPublicKeyFromPrivateKey(privKey: number): number {
    if (privKey <= 1) {
      throw new Error('private key too small')
    }
    if (privKey >= this.p) {
      throw new Error('private key too large')
    }
    return (this.g ** privKey) % this.p
  }

  getSharedSecret(privKey: number, pubKey: number): number {
    return (pubKey ** privKey) % this.p
  }
}

export default DiffieHellman
