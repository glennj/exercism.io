import java.math.BigInteger;
import static java.math.BigInteger.ONE;
import static java.math.BigInteger.TWO;

class Grains {

    BigInteger grainsOnSquare(final int square) {
        if (square < 1 || square > 64)
            throw new IllegalArgumentException("square must be between 1 and 64");
        return TWO.pow(square - 1);
    }

    BigInteger grainsOnBoard() {
        return TWO.pow(64).subtract(ONE);
    }

}
