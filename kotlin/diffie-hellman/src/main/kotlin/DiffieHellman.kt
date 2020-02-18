import java.math.BigInteger
import kotlin.random.Random

object DiffieHellman {

    fun privateKey(prime: BigInteger): BigInteger =
        BigInteger.valueOf(1L + Random.nextLong(prime.longValueExact() - 1))

    fun publicKey(p: BigInteger, g: BigInteger, privateKey: BigInteger): BigInteger =
        g.modPow(privateKey, p)

    fun secret(p: BigInteger, publicKey: BigInteger, privateKey: BigInteger): BigInteger =
        publicKey.modPow(privateKey, p)
}
