import java.time.LocalTime
import java.time.LocalDate
import java.time.LocalDateTime

class Gigasecond {

    static final int GIGASECOND = 10**9

    static add(LocalDate date) {
        add(LocalDateTime.of(date, LocalTime.MIDNIGHT))
    }

    static add(LocalDateTime datetime) {
        datetime.plusSeconds(GIGASECOND)
    }
}
