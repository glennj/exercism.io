import java.util.Arrays;

class BirdWatcher {
    private final int[] birdsPerDay;
    private static final int BUSY_COUNT = 5;

    public BirdWatcher(int[] birdsPerDay) {
        this.birdsPerDay = birdsPerDay.clone();
    }

    private int todayIndex() {
        return birdsPerDay.length - 1;
    }

    public int[] getLastWeek() {
        return birdsPerDay;
    }

    public int getToday() {
        return birdsPerDay[todayIndex()];
    }

    public void incrementTodaysCount() {
        birdsPerDay[todayIndex()]++;
    }

    public boolean hasDayWithoutBirds() {
        /*
        for (int n : birdsPerDay)
            if (n == 0)
                return true;
        return false;
        */
        return Arrays.stream(birdsPerDay).anyMatch(n -> n == 0);
    }

    public int getCountForFirstDays(int numberOfDays) {
        /*
        int count = 0;
        for (int i = 0; i < Math.min(birdsPerDay.length, numberOfDays); i++)
            count += birdsPerDay[i];
        return count;
        */
        return Arrays.stream(birdsPerDay)
                     .limit(numberOfDays)
                     .sum();
    }

    public int getBusyDays() {
        /*
        int count = 0;
        for (int n : birdsPerDay)
            if (n >= BUSY_COUNT)
                count++;
        return count;
        */
        return (int)Arrays.stream(birdsPerDay)
                          .filter(n -> n >= BUSY_COUNT)
                          .count();
    }
}
