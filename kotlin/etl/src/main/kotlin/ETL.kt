object ETL {
    fun transform(source: Map<Int, Collection<Char>>): Map<Char, Int> =
        source.flatMap { (value, tiles) ->
            tiles.map(Char::toLowerCase)
                 .map { it to value }
        }.toMap()
}
