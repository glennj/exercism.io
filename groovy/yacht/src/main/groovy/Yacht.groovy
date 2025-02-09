class Yacht {

    static Integer score(List<Integer> dice, String category) {
        switch (category) {
            case 'ones':   return single(1, dice)
            case 'twos':   return single(2, dice)
            case 'threes': return single(3, dice)
            case 'fours':  return single(4, dice)
            case 'fives':  return single(5, dice)
            case 'sixes':  return single(6, dice)

            case 'full house':      return full(dice)
            case 'four of a kind':  return four(dice)
            case 'little straight': return straight([1,2,3,4,5], dice)
            case 'big straight':    return straight([2,3,4,5,6], dice)
            case 'choice':          return dice.sum()
            case 'yacht':           return yacht(dice)

            default:                return 0;
        }
    }

    private static single(die, dice) {
        dice.findAll { it == die }.sum(0)
    }

    private static full(dice) {
        def sorted = dice.toSorted()
        (sorted[0] != sorted[4]
            && (
                (sorted[0] == sorted[1] && sorted[2] == sorted[4]) 
                || (sorted[0] == sorted[2] && sorted[3] == sorted[4]) 
            )
        ) ? dice.sum() : 0
    }

    private static four(dice) {
        def sorted = dice.toSorted()
        (sorted[0] == sorted[3] || sorted[1] == sorted[4]) 
            ? 4 * sorted[2] : 0
    }

    private static straight(wanted, dice) {
        dice.containsAll(wanted) ? 30 : 0
    }

    private static yacht(dice) {
        dice.toUnique().size() == 1 ? 50 : 0
    }
}
