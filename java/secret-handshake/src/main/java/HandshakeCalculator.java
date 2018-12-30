// With gratitude to chuckwondo
// https://exercism.io/tracks/java/exercises/secret-handshake/solutions/299fc3f3cb59469a86e6bf3a7fbe18f8

import java.util.List;
import java.util.ArrayList;
import java.util.Collections;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

class HandshakeCalculator {

    List<Signal> calculateHandshake(int number) {

        Signal[] signals = Signal.values();

        Predicate<Signal> inHandshake = (Signal s) ->
            hasBit(number, 1 << s.ordinal());

        List<Signal> handshake = IntStream
            .range(0, signals.length)
            .mapToObj(i -> signals[i])
            .filter(inHandshake)
            .collect(Collectors.toList());

        int reverse = 1 << signals.length;
        if (hasBit(number, reverse))
            Collections.reverse(handshake);

        return handshake;
    }

    private boolean hasBit(int number, int bit) {
        return (number & bit) != 0;
    }
}
