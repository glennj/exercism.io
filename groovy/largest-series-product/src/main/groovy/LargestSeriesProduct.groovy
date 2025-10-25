class LargestSeriesProduct {
    static largestProduct(digitString, span) {
        if (span > digitString.size())
            throw new  IllegalArgumentException("span must not exceed string length")
        if (span < 0)
            throw new  IllegalArgumentException("span must not be negative")

        if (span == 0)
            return 1

        def digits
        try {
            digits = digitString.collect(String::toInteger)
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("digits input must only contain digits")
        }

        series(digits, span)
            .collect { it.inject(1) { product, n -> product * n } }
            .max()
    }

    private static series(digits, span) {
        new IntRange(0, digits.size() - span)
            .collect { digits.drop(it).take(span) }

    }
}
