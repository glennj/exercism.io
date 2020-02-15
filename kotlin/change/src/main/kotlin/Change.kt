// Change making algorithm from
// http://www.ccs.neu.edu/home/jaa/CSG713.04F/Information/Handouts/dyn_prog.pdf

class ChangeCalculator(private val coins: List<Int>) {

    fun computeMostEfficientChange(amount: Int): List<Int> {
        require(amount >= 0) {"Negative totals are not allowed."}
        return when(amount) {
            0 -> emptyList()
            else -> makeChange(change(amount), amount)
        }
    }
    
    private fun makeChange(firstCoin: Map<Int, Int>, amount: Int): List<Int> {
        var amt = amount
        val result = mutableListOf<Int>()
        while (amt > 0) {
            val coin = coins[firstCoin[amt] ?:
                    throw IllegalArgumentException("The total $amount cannot be represented in the given currency.")
            ]
            result.add(coin)
            amt -= coin
        }
        return result
    }

    /* This function generates two arrays:
     *
     * min_coins:
     *     maps the minimum number of coins required to make
     *     change for each n from 1 to amount.
     *
     * first_coin:
     *     the _first_ coin used to make change for amount n
     *     (actually stores the coin _index_ into the
     *     denominations array)
     */
    private fun change(amount: Int): Map<Int, Int> {
        val minCoins = IntArray(amount + 1) {0}
        val firstCoin = mutableMapOf<Int, Int>()
        for (amt in 1..amount) {
            var min = Int.MAX_VALUE - 1
            var coin = -1

            coins.withIndex().forEach { (idx, denomination) ->
                if (denomination <= amt && 1 + minCoins[amt - denomination] < min) {
                    min = 1 + minCoins[amt - denomination]
                    coin = idx
                }
            }
            minCoins[amt] = min
            if (coin != -1) firstCoin[amt] = coin
        }
        return firstCoin
    }
}
