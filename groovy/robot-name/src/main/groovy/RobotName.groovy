import java.util.Random

class RobotName {

    private static RAND = new Random()

    String name

    RobotName() {
        reset()
    }

    def reset() {
        def name = new StringBuilder()
        name << letter() << letter() << digit() << digit() << digit()
        this.name = name.toString()
    }

    // generate an integer between 65 and 90, and coerce
    // as codepoint value into a Character
    private Character letter() { 65 + RAND.nextInt(26) }

    private int digit() { RAND.nextInt(10) }
}
