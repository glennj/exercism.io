/* Using algorithm from
 * http://www.ccs.neu.edu/home/jaa/CSG713.04F/Information/Handouts/dyn_prog.pdf
 */

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ChangeCalculator {
    private List<Integer> coins;

    public ChangeCalculator(List<Integer> coins) {
        this.coins = coins;
    }

    public List<Integer> computeMostEfficientChange(int amount) {
        if (amount == 0) {
            return Collections.emptyList();
        }
        if (amount < 0) {
            throw new IllegalArgumentException("Negative totals are not allowed.");
        }
        List<Integer> S = this.change(amount);
        return this.makeChange(S, amount);
    }

    private List<Integer> change(int amount) {
        // C: the least number of coins required for amount `n`
        List<Integer> C = new ArrayList<>(amount + 1);
        // S: the index of the first coin to use to make change for amount `n`
        List<Integer> S = new ArrayList<>(amount + 1);
        C.add(0);
        S.add(-1);

        for (int p = 1; p <= amount; p++) {
            int min = Integer.MAX_VALUE - 1;
            int coin = -1;

            for (int i = 0; i < this.coins.size(); i++) {
                int denom = coins.get(i);
                if (denom <= p) {
                    if (1 + C.get(p - denom) < min) {
                        min = 1 + C.get(p - denom);
                        coin = i;
                    }
                }
            }

            C.add(min);
            S.add(coin);
        }
        return S;
    }

    private List<Integer> makeChange(List<Integer> S, int amount) {
        if (S.get(amount) == -1) {
            throw new IllegalArgumentException(
                "The total " + amount + " cannot be represented in the given currency."
            );
        }

        List<Integer> result = new ArrayList<>();
        while (amount > 0) {
            int coin = this.coins.get(S.get(amount));
            result.add(coin);
            amount -= coin;
        }
        return result;
    }
}
