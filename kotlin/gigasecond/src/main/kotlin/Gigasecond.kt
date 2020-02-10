import java.time.LocalDate
import java.time.LocalDateTime

class Gigasecond(val startTime: LocalDateTime) {
    private val gigasecond = 1_000_000_000L

    // convert a LocalDate argument to LocalDateTime
    constructor(startDate: LocalDate) : this(startDate.atStartOfDay())

    val date: LocalDateTime = startTime.plusSeconds(gigasecond)
}
