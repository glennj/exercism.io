public class Lasagna {
    private static final int MINUTES_PER_LAYER = 2;

    public int expectedMinutesInOven() { return 40; }

    public int remainingMinutesInOven(int timeInOven) {
        return expectedMinutesInOven() - timeInOven;
    }

    public int preparationTimeInMinutes(int numberOfLayers) {
        return numberOfLayers * MINUTES_PER_LAYER;
    }

    public int totalTimeInMinutes(int numberOfLayers, int timeInOven) {
        return preparationTimeInMinutes(numberOfLayers) + timeInOven;
    }
}
