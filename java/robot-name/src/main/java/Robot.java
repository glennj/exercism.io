import java.security.SecureRandom;
import java.util.Collections;
import java.util.HashSet;
import java.util.Random;
import java.util.Set;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Stream;

public class Robot {
    private static final Set<String> allNames = Collections.synchronizedSet(new HashSet<>());
    private static final Random RAND = new SecureRandom();

    private String name;

    Robot() {
        reset();
    }

    public String getName() {
        return name;
    }

    // with gratitude to IrynaSribna
    // https://exercism.io/tracks/java/exercises/robot-name/solutions/ab1101b64f7c4b108db76d6fab64df97
    public void reset() {
        name = Stream.generate(robotName)
                .filter(unusedName)
                .findFirst()
                .get();
    }

    private Supplier<String> robotName = () -> {
        char[] chars = {alpha(), alpha(), digit(), digit(), digit()};
        return new String(chars);
    };

    // Set::add returns false if set contains element
    private Predicate<String> unusedName = name -> allNames.add(name);

    private char alpha() { return (char)('A' + (char)RAND.nextInt(26)); }
    private char digit() { return (char)('0' + (char)RAND.nextInt(10)); }
}


//    private String generateName() {
//        String newName;
//        while (true) {
//            char[] chars = {alpha(), alpha(), digit(), digit(), digit()};
//            newName = new String(chars);
//            if (!allNames.contains(newName)) {
//                allNames.add(newName);
//                break;
//            }
//        }
//        return newName;
//    }
