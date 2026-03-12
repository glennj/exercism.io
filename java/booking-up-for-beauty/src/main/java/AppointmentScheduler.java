import java.lang.StringBuilder;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

class AppointmentScheduler {
    private static final DateTimeFormatter fIn = DateTimeFormatter.ofPattern("M/d/y H:m:s");
    private static final DateTimeFormatter fOut = DateTimeFormatter.ofPattern(
            "'You have an appointment on' EEEE, MMMM d, yyyy, 'at' h:mm a.");

    public LocalDateTime schedule(String appointmentDateDescription) {
        return LocalDateTime.parse(appointmentDateDescription, fIn);
    }

    public boolean hasPassed(LocalDateTime appointmentDate) {
        return appointmentDate.compareTo(LocalDateTime.now()) < 0;        
    }

    public boolean isAfternoonAppointment(LocalDateTime appointmentDate) {
        int hour = appointmentDate.getHour();
        return 12 <= hour && hour < 18;
    }

    public String getDescription(LocalDateTime date) {
        return date.format(fOut);
    }

    public LocalDate getAnniversaryDate() {
        // anniversary day is September 15th
        return LocalDate.now().withMonth(9).withDayOfMonth(15);
    }
}
