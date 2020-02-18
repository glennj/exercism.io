object Series {

    fun slices(n: Int, s: String): List<List<Int>> {
        require(s.isNotEmpty())
        require(0 < n && n <= s.length)

/*
        return (0..s.length - n)
                .map { s.substring(it, it + n) }
                .map { it.map {c -> c - '0'} }
*/
        return s.map { it - '0' }.windowed(n)
    }
}
