import java.time.LocalTime
import java.time.LocalDate
import java.time.LocalDateTime

class Gigasecond {

    static final int GIGASECOND = 10**9

    def add(LocalDate date) {
        add(LocalDateTime.of(date, LocalTime.MIDNIGHT))
    }

    def add(LocalDateTime datetime) {
        datetime.plusSeconds(GIGASECOND)
    }
}
