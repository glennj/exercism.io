class LargestSeriesProduct {
    static largestProduct(digits, span) {
        digits
            .split("")
            .collect { Integer.parseInt(it) }
            .subsequences()
            .findAll { it.size() == span }
            .collect { it.inject(1) { product, n -> product * n } }
            .max()
        
    }
}
