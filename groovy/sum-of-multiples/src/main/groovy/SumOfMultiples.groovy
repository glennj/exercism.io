class SumOfMultiples {

    static int sum(List<Integer> factors, int limit) {
        def fs = factors.findAll { it != 0 }

        (1..<limit)
            .findAll { i -> fs.any { i % it == 0 } }
            .sum() ?: 0
    }
}
