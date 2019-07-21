class Series {

    static slices(series, sliceLength) {
        if (
            series.isEmpty() ||
            sliceLength <= 0 ||
            series.size() < sliceLength
        )
            throw new ArithmeticException()

        series.toList()
              .collate(sliceLength, 1, false)
              *.join("")
    }
}
