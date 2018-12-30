import java.util.Map;
import java.util.HashMap;

class Yacht {
    private final YachtCategory category;
    private final Map<Integer,Integer> histogram;
    private int sumOfDice;

    Yacht(int[] dice, YachtCategory yachtCategory) {
        category = yachtCategory;
        histogram = new HashMap<>();
        sumOfDice = 0;
        for (int d = 1; d <= 6; d++) {
            histogram.put(d, 0);
        }
        for (int d : dice) {
            histogram.put(d, histogram.get(d) + 1);
            sumOfDice += d;
        }
    }

    int score() {
        switch (category) {
            case ONES:            return sumOf(1);
            case TWOS:            return sumOf(2);
            case THREES:          return sumOf(3);
            case FOURS:           return sumOf(4);
            case FIVES:           return sumOf(5);
            case SIXES:           return sumOf(6);
            case FULL_HOUSE:      return fullHouse();
            case FOUR_OF_A_KIND:  return fourOfAKind();
            case LITTLE_STRAIGHT: return straight(1);
            case BIG_STRAIGHT:    return straight(2);
            case YACHT:           return yacht();
            case CHOICE:          return sumOfDice;
        }
        return 0;
    }

    private int sumOf(int die) {
        return die * histogram.get(die);
    }

    private int fullHouse() {
        boolean hasFull = histogram.containsValue(3) && histogram.containsValue(2);
        return hasFull ? sumOfDice : 0;
    }

    private int fourOfAKind() {
        for (Map.Entry<Integer,Integer> entry : histogram.entrySet()) {
            if (entry.getValue() >= 4)
                return 4 * entry.getKey();
        }
        return 0;
    }

    private int yacht() {
        return histogram.containsValue(5) ? 50 : 0;
    }

    private int straight(int start) {
        boolean hasStraight = 
            histogram.get(2) > 0 &&
            histogram.get(3) > 0 &&
            histogram.get(4) > 0 &&
            histogram.get(5) > 0 && (
                (start == 1 && histogram.get(1) > 0) ||
                (start == 2 && histogram.get(6) > 0)
            );
        return hasStraight ? 30 : 0;
    }
}
