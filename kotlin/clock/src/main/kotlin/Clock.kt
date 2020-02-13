class Clock(hour: Int, minute: Int) {

    private var minuteOfDay: Int = 0

    init {
        minuteOfDay = hour * 60 + minute
        normalize()
    }

    private fun normalize() {
        minuteOfDay = minuteOfDay.posMod(24 * 60)
    }

    override fun toString(): String =
            String.format("%02d:%02d", minuteOfDay / 60, minuteOfDay % 60)

    override fun equals(other: Any?) = when(other) {
        is Clock -> this.minuteOfDay == other.minuteOfDay
        else -> false
    }

    override fun hashCode(): Int = minuteOfDay

    fun add(minutes: Int) {
        minuteOfDay += minutes
        normalize()
    }

    fun subtract(minutes: Int) {
        add(minutes * -1)
    }
}

// get (-12 "mod" 10) to report result in `0 until divisor`
// i.e. +8, not -2
fun Int.posMod(divisor: Int): Int {
    require(divisor > 0)
    return ((this % divisor) + divisor) % divisor
}
