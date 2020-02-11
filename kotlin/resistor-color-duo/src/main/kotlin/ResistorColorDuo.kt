object ResistorColorDuo {

    // we're not worried about invalid colors, or too few colors.
    fun value(vararg colors: Color): Int =
        colors.slice(0..1).fold(0) { acc, color ->
            acc * 10 + color.ordinal
        }
}
