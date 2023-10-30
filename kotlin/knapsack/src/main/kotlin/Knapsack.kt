import kotlin.math.max

data class Item(val weight: Int, val value: Int)

// returns the maximum value that can be stored in this knapsack
fun knapsack(maximumWeight: Int, items: List<Item>): Int {
    // "matrix" m will eventually hold the maximum value
    val m = Array(items.size + 1) {_ -> Array(maximumWeight + 1){0}}

    for ((i, item) in items.withIndex())
        for (j in 1..maximumWeight)
            m[i + 1][j] =
                if (item.weight > j)
                    m[i][j]
                else
                    max(m[i][j], item.value + m[i][j - item.weight])

    return m[items.size][maximumWeight]
}
