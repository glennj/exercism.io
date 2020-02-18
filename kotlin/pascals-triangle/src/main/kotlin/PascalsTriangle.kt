object PascalsTriangle {
    fun computeTriangle(rows: Int): List<List<Int>> =
        (0 until rows).map { n ->
            (0..n).map { k -> n.binom(k).toInt() }
        }
}

// binomial coefficient
fun Int.binom(k: Int): Long =
    when {
        (k == 0 || this - k == 0) -> 1L
        (k == 1 || this - k == 1) -> this.toLong()
        else -> this.factorial() / k.factorial() / (this - k).factorial()
    }

tailrec fun Int.factorial(): Long =
    if (this == 0) 1L else this * (this - 1).factorial()


/* my first take
 *
    fun computeTriangle(rows: Int): List<List<Int>> {
        if (rows == 0) return emptyList()

        val pascal = ArrayList<List<Int>>(rows)
        pascal.add(listOf(1))

        for (i in 1 until rows) {
            val row = ArrayList<Int>()
            for (j in 0..i) {
                val a = if (j == 0) 0 else pascal[i - 1][j - 1]
                val b = if (j == i) 0 else pascal[i - 1][j]
                row.add(a + b)
            }
            pascal.add(row)
        }
        return pascal
    }
 */

/* community solutions
 *
 * https://exercism.io/tracks/kotlin/exercises/pascals-triangle/solutions/6edc64642be94aa09e45aa3c9705be54
 *
  fun computeTriangle(rows: Int): List<List<Int>> {
    require(rows >= 0) { "Rows can't be negative!" }

    return generateSequence(listOf(1)) { prev ->
      listOf(1) + prev.windowed(2).map { it.sum() } + listOf(1)
    }.take(rows).toList()
  }

 */
