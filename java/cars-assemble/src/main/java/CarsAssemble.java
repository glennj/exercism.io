public class CarsAssemble {
	private static final int CARS_PER_HOUR = 221;
	private static final int MINUTES_PER_HOUR = 60;

    public double productionRatePerHour(int speed) {
		int rate = CARS_PER_HOUR * speed;

        if (1 <= speed && speed <= 4)
			return 1.0 * rate;
		else if (5 <= speed && speed <= 8)
			return 0.9 * rate;
		else if (speed == 9)
			return 0.8 * rate;
		else
			return 0.77 * rate;
    }

    public int workingItemsPerMinute(int speed) {
        return (int)(productionRatePerHour(speed) / MINUTES_PER_HOUR);
    }
}
