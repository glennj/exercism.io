object Hamming {

    private val errMsg: String = "left and right strands must be of equal length"

    // third take: reduce, require
    fun compute(leftStrand: String, rightStrand: String): Int {
        require(leftStrand.length == rightStrand.length) { errMsg }
        return leftStrand.indices.fold(0) { dist, i ->
            when (leftStrand[i]) {
                rightStrand[i] -> dist
                else -> dist + 1
            }
        }
    }
}

/*

// first take: simple for loop
fun compute(leftStrand: String, rightStrand: String): Int {
    if (leftStrand.length != rightStrand.length) {
        throw IllegalArgumentException(errMsg)
    }

    var distance = 0
    for (i in leftStrand.indices) {
        if (leftStrand[i] != rightStrand[i]) {
            distance++
        }
    }
    return distance
}

// second take, more functional
fun compute(leftStrand: String, rightStrand: String): Int {
    if (leftStrand.length != rightStrand.length) throw IllegalArgumentException(errMsg)
    return leftStrand.indices.filter { i -> leftStrand[i] != rightStrand[i] }.size
}

// consensus community solution
fun compute(leftStrand: String, rightStrand: String): Int {
    require(leftStrand.length == rightStrand.length) { errMsg }
    return leftStrand.zip(rightStrand).count { (a, b) -> a != b }
}

 */
