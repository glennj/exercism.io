import java.util.Arrays;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

class PokerHand implements Comparable<PokerHand> {
    private String origin;
    private List<Card> cards;

    // this determines the "value" of this hand, 9 is best (5 of a kind)
    private int value;

    // we keep a list of card values to break ties (e.g. pair of 8s, K high)
    private List<Integer> cardRanking;

    // cardGroupings is a list of (numCards, cardValue) pairs
    // The `numCards` is particularly important as we are interested in
    // "n-of-a-kind" poker hands.
    private static class Grouping {
        private int count;
        private int cardValue;
        Grouping(int count, int cardValue) {
            this.count = count;
            this.cardValue = cardValue;
        }
        int getCount() { return count; }
        int getCardValue() { return cardValue; }
    }

    private List<Grouping> cardGroupings;

    /* ********************************************************************** */
    PokerHand(String input) {
        origin = input;
        cards = Arrays
                .stream(input.split("\\s+"))
                .map(Card::new)
                .collect(Collectors.toList());

        cardGroupings = findGroupings();
        evaluateHand();
    }

    /* ********************************************************************** */
    @Override
    public String toString() { return origin; }

    int getValue() { return value; }

    // if 2 hands have the same value (e.g. two straights), we need to
    // compare the cardRanking's of them. Reduce the rankings to a numerical value
    int getRankingValue() {
        int value = 0;
        for (int v : cardRanking) {
            value = value * Card.FACES.size() + v;
        }
        return value;
    }

    /* ********************************************************************** */
    @Override
    public int compareTo(PokerHand other) {
        int cmp = Integer.compare(value, other.value);
        if (cmp == 0)
            cmp = Integer.compare(getRankingValue(), other.getRankingValue());
        return cmp;
    }

    /* ********************************************************************** */
    private List<Grouping> findGroupings() {
        return cards
                .stream()
                .collect(Collectors.groupingBy(card -> card.getValue()))
                .entrySet()
                .stream()
                .map(e -> new Grouping(e.getValue().size(), e.getKey()))
                .sorted(Comparator
                        .comparingInt(Grouping::getCount)
                        .thenComparingInt(Grouping::getCardValue))
                .collect(Collectors.toList());
    }

    /* ********************************************************************** */
    private void evaluateHand() {
        // the following methods will set the cardRanking as a side effect
        if      (isFiveOfAKind())    { value = 9; }
        else if (isStraightFlush())  { value = 8; }
        else if (isFourOfAKind())    { value = 7; }
        else if (isFullHouse())      { value = 6; }
        else if (isFlush())          { value = 5; }
        else if (isStraight())       { value = 4; }
        else if (isThreeOfAKind())   { value = 3; }
        else if (isTwoPairs())       { value = 2; }
        else if (isOnePair())        { value = 1; }
        else {
            value = 0;
            cardRanking = cardValuesDescending();
        }
    }

    /* ********************************************************************** */
    private boolean isFiveOfAKind() {
        boolean result = cardGroupings.size() == 1;
        if (result) {
            cardRanking = Arrays.asList(cards.get(0).getValue());
        }
        return result;
    }

    private boolean isStraightFlush() {
        // test in this order: if 2 straight flushes, winner goes to high card of straight.
        return isFlush() && isStraight();
    }

    private boolean isFlush() {
        String suit = cards.get(0).getSuit();
        boolean result = cards.stream().allMatch(card -> card.getSuit().equals(suit));
        if (result)
            cardRanking = cardValuesDescending();
        return result;
    }

    private boolean isStraight() {
        // Ace can be high or low for this hand.
        List<Integer> values = cardValuesDescending();
        boolean straight = true;
        for (int i = 0; i < 4; i++) {
            if (values.get(i) != values.get(i + 1) + 1) {
                straight = false;
                break;
            }
        }
        if (straight) {
            cardRanking = Arrays.asList(values.get(0));
            return true;
        }

        // check for A,2,3,4,5 straight
        straight = values.containsAll(Arrays.asList(12,0,1,2,3));
        if (straight)
            cardRanking = Arrays.asList(3); // value of card face "5"
        return straight;
    }

    /* ********************************************************************** */
    private boolean byCardGrouping(List<Integer> groupCounts) {
        List<Integer> handGroupCounts = cardGroupings
                .stream()
                .map(Grouping::getCount)
                .sorted(Comparator.reverseOrder())
                .collect(Collectors.toList());

        boolean result = handGroupCounts.equals(groupCounts);

        if (result) {
            cardRanking = new ArrayList<>();
            groupCounts
                    .stream() // they are already sorted in order of importance
                    .distinct()
                    .forEach(n -> {
                           cardGroupings
                                    .stream()
                                    .filter(grouping -> grouping.getCount() == n)
                                    .map(Grouping::getCardValue)
                                    .sorted(Comparator.reverseOrder())
                                    .forEach(val -> cardRanking.add(val));
                    });
        }

        return result;
    }

    private boolean isFourOfAKind() {
        return byCardGrouping(Arrays.asList(4, 1));
    }

    private boolean isFullHouse() {
        return byCardGrouping(Arrays.asList(3, 2));
    }

    private boolean isThreeOfAKind() {
        return byCardGrouping(Arrays.asList(3, 1, 1));
    }

    private boolean isTwoPairs() {
        return byCardGrouping(Arrays.asList(2, 2, 1));
    }

    private boolean isOnePair() {
        return byCardGrouping(Arrays.asList(2, 1, 1, 1));
    }

    /* ********************************************************************** */
    List<Integer> cardValuesDescending() {
        return cardGroupings.stream()
                .map(Grouping::getCardValue)
                .sorted(Comparator.reverseOrder())
                .collect(Collectors.toList());
    }
}
