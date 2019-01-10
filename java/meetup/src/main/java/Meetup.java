import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.DayOfWeek;
import java.time.YearMonth;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

public class Meetup {
    private final Map<DayOfWeek, LinkedList<LocalDate>> days;

    Meetup(int month, int year) throws DateTimeException {
        YearMonth ym = YearMonth.of(year, month);
        days = new HashMap<>();
        for (DayOfWeek d : DayOfWeek.values()) {
            days.put(d, new LinkedList<>());
        }
        fillDays(ym);
    }

    public LocalDate day(DayOfWeek dow, MeetupSchedule nth) {
        switch (nth) {
            case FIRST:  return days.get(dow).get(0);
            case SECOND: return days.get(dow).get(1);
            case THIRD:  return days.get(dow).get(2);
            case FOURTH: return days.get(dow).get(3);
            case LAST:   return days.get(dow).getLast();
            case TEENTH:
                return days.get(dow)
                        .stream()
                        .filter(d -> d.getDayOfMonth() >= 13)
                        .findFirst()
                        .orElseThrow(() -> new DateTimeException("month has less than 13 days."));
            default:
                // should not get here.
                return LocalDate.now();
        }
    }

    private void fillDays(YearMonth ym) {
        LocalDate day = ym.atDay(1);
        int lastDayOfMonth = ym.atEndOfMonth().getDayOfMonth();
        while (true) {
            days.get(day.getDayOfWeek()).add(day);
            if (day.getDayOfMonth() == lastDayOfMonth)
                break;
            day = day.plusDays(1);
        }
    }
}
