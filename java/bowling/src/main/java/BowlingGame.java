import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

class BowlingGame {

    private int score = 0;
    private int frame = 1;
    private List<Integer> current = new ArrayList<>(); // balls rolled in the current frame
    private List<Integer> bonuses = new ArrayList<>(); // pending spare/strike bonuses

    // we encounter exceptional circumstances during the rolls,
    // but we don't want to throw them until checking the score.
    private IllegalStateException pendingException;

    /* ********************************************************************** */
    int score() {
        if (pendingException != null)
            throw pendingException;
        if (!isGameOver())
            throw new IllegalStateException("Score cannot be taken until the end of the game");

        return score;
    }

    /* ********************************************************************** */
    void roll(int roll) {
        // If we have hit an exception, it doesn't matter about any subsequent rolls
        if (pendingException != null) return;

        if (isGameOver()) {
            pendingException = new IllegalStateException("Cannot roll after game is over");
            return;
        }
        if (roll < 0) {
            pendingException = new IllegalStateException("Negative roll is invalid");
            return;
        }
        if (isTooManyPins(roll)) {
            pendingException = new IllegalStateException("Pin count exceeds pins on the lane");
            return;
        }

        addToScore(roll);
        updateFrameStatus(roll);
    }

    private void addToScore(int roll) {
        // add this roll
        score += roll;
        // then check for previous spare/strike to add it again
        bonuses = bonuses
                .stream()
                .peek(i -> score += roll)
                .map(i -> i - 1)
                .filter(i -> i > 0)
                .collect(Collectors.toList());
    }

    private void updateFrameStatus(int roll) {
        if (is10thFrame())
            update10thFrameStatus(roll);
        else
            updateNthFrameStatus(roll);
    }

    private void updateNthFrameStatus(int roll) {
        if (roll == 10) {
            frame++;
            bonuses.add(2);
        }
        else if (current.isEmpty()) {
            current.add(roll);
        }
        else {
            if (current.get(0) + roll == 10)
                bonuses.add(1);
            frame += 1;
            current = new ArrayList<>();
        }
    }

    private void update10thFrameStatus(int roll) {
        current.add(roll);
        if (current.size() == 3 || (current.size() == 2 && sum(current) < 10))
            frame += 1;
    }

    private int sum(List<Integer> intList) {
        return intList.stream().mapToInt(Integer::intValue).sum();
    }

    private boolean isGameOver() {
        return frame > 10;
    }

    private boolean is10thFrame() {
        return frame == 10;
    }

    private boolean isTooManyPins(int roll) {
        if (roll > 10) return true;
        if (current.isEmpty()) return false;
        if (is10thFrame()) return isTooManyPins10th(roll);
        return current.get(0) + roll > 10;
    }

    private boolean isTooManyPins10th(int roll) {
        List<Integer> nonStrikes = current
                .stream()
                .filter(i -> i < 10)
                .collect(Collectors.toList());
        if (nonStrikes.isEmpty()) return false;
        if (nonStrikes.size() == 1) return nonStrikes.get(0) + roll > 10;
        return sum(nonStrikes) != 10;
    }
}
