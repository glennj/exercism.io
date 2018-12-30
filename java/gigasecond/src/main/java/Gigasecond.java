import java.time.LocalDate;
import java.time.LocalDateTime;

class Gigasecond {

    LocalDateTime birth;
    static long GIGASECOND = 1_000_000_000L;

    Gigasecond(LocalDate birthDate) {
        birth = birthDate.atStartOfDay();
    }

    Gigasecond(LocalDateTime birthDateTime) {
        birth = birthDateTime;
    }

    LocalDateTime getDateTime() {
        return birth.plusSeconds(GIGASECOND);
    }
}
