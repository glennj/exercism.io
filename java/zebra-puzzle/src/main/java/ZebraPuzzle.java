import java.util.List;

class ZebraPuzzle {
    private final int FIRST = 1;
    private final int MIDDLE = 3;

    private String waterDrinker, zebraOwner;

    // instance variables set by the various methods
    private int red, green, ivory, yellow, blue;                            // houses
    private int english, spanish, ukranian, norwegian, japanese;            // nationalities
    private int dog, snails, fox, horse, zebra;                             // pets
    private int oldGold, kools, chesterfields, luckyStrike, parliaments;    // smokes
    private int coffee, tea, milk, orangeJuice, water;                      // beverages

    private String[] nationalities = {"", "", "", "", "", ""};

    // No need to get fancy, just hardcode them
    private final List<List<Integer>> permutations = List.of(
            List.of(1, 2, 3, 4, 5), List.of(1, 2, 3, 5, 4), List.of(1, 2, 4, 3, 5), List.of(1, 2, 4, 5, 3),
            List.of(1, 2, 5, 3, 4), List.of(1, 2, 5, 4, 3), List.of(1, 3, 2, 4, 5), List.of(1, 3, 2, 5, 4),
            List.of(1, 3, 4, 2, 5), List.of(1, 3, 4, 5, 2), List.of(1, 3, 5, 2, 4), List.of(1, 3, 5, 4, 2),
            List.of(1, 4, 2, 3, 5), List.of(1, 4, 2, 5, 3), List.of(1, 4, 3, 2, 5), List.of(1, 4, 3, 5, 2),
            List.of(1, 4, 5, 2, 3), List.of(1, 4, 5, 3, 2), List.of(1, 5, 2, 3, 4), List.of(1, 5, 2, 4, 3),
            List.of(1, 5, 3, 2, 4), List.of(1, 5, 3, 4, 2), List.of(1, 5, 4, 2, 3), List.of(1, 5, 4, 3, 2),
            List.of(2, 1, 3, 4, 5), List.of(2, 1, 3, 5, 4), List.of(2, 1, 4, 3, 5), List.of(2, 1, 4, 5, 3),
            List.of(2, 1, 5, 3, 4), List.of(2, 1, 5, 4, 3), List.of(2, 3, 1, 4, 5), List.of(2, 3, 1, 5, 4),
            List.of(2, 3, 4, 1, 5), List.of(2, 3, 4, 5, 1), List.of(2, 3, 5, 1, 4), List.of(2, 3, 5, 4, 1),
            List.of(2, 4, 1, 3, 5), List.of(2, 4, 1, 5, 3), List.of(2, 4, 3, 1, 5), List.of(2, 4, 3, 5, 1),
            List.of(2, 4, 5, 1, 3), List.of(2, 4, 5, 3, 1), List.of(2, 5, 1, 3, 4), List.of(2, 5, 1, 4, 3),
            List.of(2, 5, 3, 1, 4), List.of(2, 5, 3, 4, 1), List.of(2, 5, 4, 1, 3), List.of(2, 5, 4, 3, 1),
            List.of(3, 1, 2, 4, 5), List.of(3, 1, 2, 5, 4), List.of(3, 1, 4, 2, 5), List.of(3, 1, 4, 5, 2),
            List.of(3, 1, 5, 2, 4), List.of(3, 1, 5, 4, 2), List.of(3, 2, 1, 4, 5), List.of(3, 2, 1, 5, 4),
            List.of(3, 2, 4, 1, 5), List.of(3, 2, 4, 5, 1), List.of(3, 2, 5, 1, 4), List.of(3, 2, 5, 4, 1),
            List.of(3, 4, 1, 2, 5), List.of(3, 4, 1, 5, 2), List.of(3, 4, 2, 1, 5), List.of(3, 4, 2, 5, 1),
            List.of(3, 4, 5, 1, 2), List.of(3, 4, 5, 2, 1), List.of(3, 5, 1, 2, 4), List.of(3, 5, 1, 4, 2),
            List.of(3, 5, 2, 1, 4), List.of(3, 5, 2, 4, 1), List.of(3, 5, 4, 1, 2), List.of(3, 5, 4, 2, 1),
            List.of(4, 1, 2, 3, 5), List.of(4, 1, 2, 5, 3), List.of(4, 1, 3, 2, 5), List.of(4, 1, 3, 5, 2),
            List.of(4, 1, 5, 2, 3), List.of(4, 1, 5, 3, 2), List.of(4, 2, 1, 3, 5), List.of(4, 2, 1, 5, 3),
            List.of(4, 2, 3, 1, 5), List.of(4, 2, 3, 5, 1), List.of(4, 2, 5, 1, 3), List.of(4, 2, 5, 3, 1),
            List.of(4, 3, 1, 2, 5), List.of(4, 3, 1, 5, 2), List.of(4, 3, 2, 1, 5), List.of(4, 3, 2, 5, 1),
            List.of(4, 3, 5, 1, 2), List.of(4, 3, 5, 2, 1), List.of(4, 5, 1, 2, 3), List.of(4, 5, 1, 3, 2),
            List.of(4, 5, 2, 1, 3), List.of(4, 5, 2, 3, 1), List.of(4, 5, 3, 1, 2), List.of(4, 5, 3, 2, 1),
            List.of(5, 1, 2, 3, 4), List.of(5, 1, 2, 4, 3), List.of(5, 1, 3, 2, 4), List.of(5, 1, 3, 4, 2),
            List.of(5, 1, 4, 2, 3), List.of(5, 1, 4, 3, 2), List.of(5, 2, 1, 3, 4), List.of(5, 2, 1, 4, 3),
            List.of(5, 2, 3, 1, 4), List.of(5, 2, 3, 4, 1), List.of(5, 2, 4, 1, 3), List.of(5, 2, 4, 3, 1),
            List.of(5, 3, 1, 2, 4), List.of(5, 3, 1, 4, 2), List.of(5, 3, 2, 1, 4), List.of(5, 3, 2, 4, 1),
            List.of(5, 3, 4, 1, 2), List.of(5, 3, 4, 2, 1), List.of(5, 4, 1, 2, 3), List.of(5, 4, 1, 3, 2),
            List.of(5, 4, 2, 1, 3), List.of(5, 4, 2, 3, 1), List.of(5, 4, 3, 1, 2), List.of(5, 4, 3, 2, 1)
    );

    String getWaterDrinker() {
        if (this.waterDrinker == null)
            this.solve();
        return this.waterDrinker;
    }

    String getZebraOwner() {
        if (this.zebraOwner == null)
            this.solve();
        return this.zebraOwner;
    }

    private void solve() {
       for (List<Integer> perm : this.permutations)
            if (this.solveForColour(perm))
                break;
    }

    private boolean solveForColour(List<Integer> permutation) {
        this.red = permutation.get(0);
        this.green = permutation.get(1);
        this.ivory = permutation.get(2);
        this.yellow = permutation.get(3);
        this.blue = permutation.get(4);

        if (this.rightOf(this.green, this.ivory))       // clue 6
            for (List<Integer> perm : this.permutations)
                if (this.solveForNationality(perm))
                    return true;
        return false;
    }

    private boolean solveForNationality(List<Integer> permutation) {
        this.english = permutation.get(0);
        this.spanish = permutation.get(1);
        this.ukranian = permutation.get(2);
        this.norwegian = permutation.get(3);
        this.japanese = permutation.get(4);

        if (this.english == this.red                    // clue 2
            && this.norwegian == this.FIRST             // clue 10
            && this.nextTo(this.norwegian, this.blue)   // clue 15
        ) {
            this.nationalities[this.english] = "EnglishMan";
            this.nationalities[this.spanish] = "Spanish";
            this.nationalities[this.ukranian] = "Ukranian";
            this.nationalities[this.norwegian] = "Norwegian";
            this.nationalities[this.japanese] = "Japanese";

            for (List<Integer> perm : this.permutations)
                if (this.solveForBeverage(perm))
                    return true;
        }
        return false;
    }

    private boolean solveForBeverage(List<Integer> permutation) {
        this.coffee = permutation.get(0);
        this.tea = permutation.get(1);
        this.milk = permutation.get(2);
        this.orangeJuice = permutation.get(3);
        this.water = permutation.get(4);

        if (this.coffee == this.green                   // clue 4
            && this.ukranian == this.tea                // clue 5
            && this.milk == this.MIDDLE                 // clue 9
        )
            for (List<Integer> perm : this.permutations)
                if (this.solveForSmokes(perm))
                    return true;
        return false;
    }

    private boolean solveForSmokes(List<Integer> permutation) {
        this.oldGold = permutation.get(0);
        this.kools = permutation.get(1);
        this.chesterfields = permutation.get(2);
        this.luckyStrike = permutation.get(3);
        this.parliaments = permutation.get(4);

        if (this.kools == this.yellow                   // clue 8
            && this.luckyStrike == this.orangeJuice     // clue 13
            && this.japanese == this.parliaments        // clue 14
        )
            for (List<Integer> perm : this.permutations)
                if (this.solveForPets(perm))
                    return true;
        return false;
    }

    private boolean solveForPets(List<Integer> permutation) {
        this.dog = permutation.get(0);
        this.snails = permutation.get(1);
        this.fox = permutation.get(2);
        this.horse = permutation.get(3);
        this.zebra = permutation.get(4);

        if (this.spanish == this.dog                     // clue 3
            && this.oldGold == this.snails               // clue 7
            && this.nextTo(this.chesterfields, this.fox) // clue 11
            && this.nextTo(this.kools, this.horse)       // clue 12
        ) {
            this.waterDrinker = this.nationalities[this.water];
            this.zebraOwner = this.nationalities[this.zebra];
            return true;
        }
        return false;
    }

    private boolean rightOf(int a, int b) {
        return a == b + 1;
    }

    private boolean nextTo(int a, int b) {
        return this.rightOf(a, b) || this.rightOf(b, a);
    }
}
