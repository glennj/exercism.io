class ETL {

    static transform(input) {
        input.inject([:]) { result, points, letters ->
            def p = points as int
            letters.each { result[it.toLowerCase()] = p }
            result
        }
    }
}
