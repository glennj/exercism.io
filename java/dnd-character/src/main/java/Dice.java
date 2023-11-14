import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

class Dice {
    private final int sides;
    private final int num;
    private List<Integer> rolls;

    Dice(int sides, int num) {
        this.sides = sides;
        this.num = num;
    }

    static Dice of(int sides, List<Integer> nums) {
        Dice dice = new Dice(sides, nums.size());
        dice.setDice(nums);
        return dice;
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

    List<Integer> getDice() {
        return List.copyOf(rolls);
    }

    void setDice(List<Integer> nums) {
        rolls = nums.stream().sorted().collect(Collectors.toList());
    }

    int sumOfLargest(int m) {
        return sumOf(largest(m));
    }

    List<Integer> largest(int m) {
        if (m > num) throw new IllegalArgumentException();
        return rolls.subList(num - m, num);
    }

    private int sumOf(List<Integer> values) {
        return values.stream().mapToInt(Integer::intValue).sum();
    }
}
