import java.util.Objects;

class Clock {
    private int hour;
    private int minute;

    Clock(int hour, int minute) {
        this.hour = hour;
        this.minute = minute;
        normalize();
    }

    private void normalize() {
        int hoursAdjustment = Math.floorDiv(minute, 60);
        minute = Math.floorMod(minute, 60);
        hour = Math.floorMod(hour + hoursAdjustment, 24);
    }

    @Override
    public String toString() {
        return String.format("%02d:%02d", hour, minute);
    }

    void add(int minutes) {
        minute += minutes;
        normalize();
    }

    @Override
    public boolean equals(Object o) {
        if (o == null) return false;
        if (o == this) return true;
        if (o instanceof Clock) {
            Clock other = (Clock) o;
            return hour == other.hour && minute == other.minute;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(hour, minute);
    }
}
