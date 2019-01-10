import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

class DnDCharacter {

    private enum Characteristic {
        STRENGTH, DEXTERITY, CONSTITUTION, INTELLIGENCE, WISDOM, CHARISMA
    }

    private final Map<Characteristic, Integer> abilities = new HashMap<>();
    private int hitpoints;

    DnDCharacter() {
        for (Characteristic c : Characteristic.values()) {
            abilities.put(c, ability());
        }
        hitpoints = 10 + modifier(getConstitution());
    }

    int getStrength()     { return abilities.get(Characteristic.STRENGTH); }
    int getDexterity()    { return abilities.get(Characteristic.DEXTERITY); }
    int getConstitution() { return abilities.get(Characteristic.CONSTITUTION); }
    int getIntelligence() { return abilities.get(Characteristic.INTELLIGENCE); }
    int getWisdom()       { return abilities.get(Characteristic.WISDOM); }
    int getCharisma()     { return abilities.get(Characteristic.CHARISMA); }
    int getHitpoints()    { return hitpoints; }

    int modifier(int input) {
        return (int) Math.floor((input - 10) / 2.0);
    }

    /**
     * Throw 4 6-sided dice and return the sum of the largest 3.
     */
    int ability() {
        return Dice._4d6().sumOfLargest(3);
    }

    /*
     */
    private static class Dice {
        private final int sides;
        private final int num;
        private List<Integer> rolls;

        Dice(int sides, int num) {
            this.sides = sides;
            this.num = num;
        }

        static Dice _4d6() {
            return new Dice(6, 4).roll();
        }

        Dice roll() {
            rolls = IntStream
                    .rangeClosed(1, num)
                    .map(i -> roll1())
                    .boxed()
                    .sorted()
                    .collect(Collectors.toList());
            return this;
        }

        private int roll1() {
            return 1 + new Random().nextInt(sides);
        }

        int sumOfLargest(int m) {
            return sumOf(largest(m));
        }

        List<Integer> largest(int m) {
            if (m > num) throw new IllegalArgumentException();
            return rolls.subList(num - m, num);
        }

        // other utility methods.
        // int sum() { return sumOf(rolls); }
        // int max() { return rolls.get(rolls.size() - 1); }
        // int min() { return rolls.get(0); }

        private int sumOf(List<Integer> values) {
            return values.stream().mapToInt(Integer::intValue).sum();
        }
    }
}
