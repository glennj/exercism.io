public class CarsAssemble {
    private static final int CARS_PER_HOUR = 221;
    private static final int MINUTES_PER_HOUR = 60;

    private double successRate(int speed) {
        if (1 <= speed && speed <= 4)  return 1.0;
        if (5 <= speed && speed <= 8)  return 0.9;
        if (speed == 9)                return 0.8;
        return 0.77;
    }

    public double productionRatePerHour(int speed) {
        return CARS_PER_HOUR * speed * successRate(speed);
    }

    public int workingItemsPerMinute(int speed) {
        return (int)(productionRatePerHour(speed) / MINUTES_PER_HOUR);
    }
}
