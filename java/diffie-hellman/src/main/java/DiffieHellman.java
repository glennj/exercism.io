import java.math.BigInteger;
import java.util.Random;

class DiffieHellman {

    BigInteger privateKey(BigInteger prime) {
        // random number between 1 (inclusive) and prime (exclusive)
        int rand = 1 + new Random().nextInt(prime.intValue() - 1);
        return BigInteger.valueOf(rand);
    }

    BigInteger publicKey(BigInteger p, BigInteger g, BigInteger privateKey) {
        return g.modPow(privateKey, p);
    }

    BigInteger secret(BigInteger p, BigInteger publicKey, BigInteger privateKey) {
        return publicKey.modPow(privateKey, p);
    }
}
