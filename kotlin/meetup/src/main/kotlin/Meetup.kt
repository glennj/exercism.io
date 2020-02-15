import java.time.DayOfWeek
import java.time.LocalDate

class Meetup(private val month: Int, private val year: Int) {

    /* The typical implementation

    fun day(dayOfWeek: DayOfWeek, schedule: MeetupSchedule): LocalDate {
        val ym = java.time.YearMonth.of(year, month)

        // get the month's days for the requested day of week
        val days = (1..ym.lengthOfMonth())
                .map { ym.atDay(it) }
                .filter { it.dayOfWeek == dayOfWeek }

        return when(schedule) {
            MeetupSchedule.FIRST  -> days[0]
            MeetupSchedule.SECOND -> days[1]
            MeetupSchedule.THIRD  -> days[2]
            MeetupSchedule.FOURTH -> days[3]
            MeetupSchedule.LAST   -> days.last()
            MeetupSchedule.TEENTH -> days.first { it.dayOfMonth >= 13 }
        }
    }
     */


    /* Alternately: start at the first day of the schedule period, and
     * iterate forward until the desired day is found
     */

    fun day(dayOfWeek: DayOfWeek, schedule: MeetupSchedule): LocalDate {

        var date = when(schedule) {
            MeetupSchedule.FIRST  -> LocalDate.of(year, month, 1)
            MeetupSchedule.SECOND -> LocalDate.of(year, month, 8)
            MeetupSchedule.TEENTH -> LocalDate.of(year, month, 13)
            MeetupSchedule.THIRD  -> LocalDate.of(year, month, 15)
            MeetupSchedule.FOURTH -> LocalDate.of(year, month, 22)

            MeetupSchedule.LAST   -> LocalDate.of(year, month, 1)
                                        .plusMonths(1)
                                        .minusDays(7)
        }

        while (date.dayOfWeek != dayOfWeek) {
            date = date.plusDays(1)
        }

        return date
    }
}
