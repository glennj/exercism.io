import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

class Poker {
    private List<PokerHand> hands;

    Poker(List<String> handStrings) {
        hands = handStrings
                .stream()
                .map(PokerHand::new)
                .collect(Collectors.toList());
    }

    List<String> getBestHands() {
        List<PokerHand> bestHands = getBestHandsBy(hands, PokerHand::getValue);

        if (bestHands.size() > 1)
            bestHands = getBestHandsBy(bestHands, PokerHand::getRankingValue);

        return bestHands
                .stream()
                .map(PokerHand::toString)
                .collect(Collectors.toList());
    }

    List<PokerHand> getBestHandsBy(List<PokerHand> hands, Function<PokerHand, Integer> valueFunc) {
        int max = -1;
        List<PokerHand> bestHands = new ArrayList<>();
        for (PokerHand hand : hands) {
            int value = valueFunc.apply(hand);
            if (value > max) {
                max = value;
                bestHands = new ArrayList<>();
            }
            if (value == max) {
                bestHands.add(hand);
            }
        }
        return bestHands;
    }
}
