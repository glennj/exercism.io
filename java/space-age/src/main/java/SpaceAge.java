import java.util.Map;
import java.util.HashMap;

class SpaceAge {
    private static final double SECONDS_PER_EARTH_YEAR = 31_557_600;

    private static final Map<String, Double> RELATIVE_YEAR;
    static {
        RELATIVE_YEAR = new HashMap<>();
        RELATIVE_YEAR.put("Mercury", 0.2408467);
        RELATIVE_YEAR.put("Venus", 0.61519726);
        RELATIVE_YEAR.put("Mars", 1.8808158);
        RELATIVE_YEAR.put("Jupiter", 11.862615);
        RELATIVE_YEAR.put("Saturn", 29.447498);
        RELATIVE_YEAR.put("Uranus", 84.016846);
        RELATIVE_YEAR.put("Neptune", 164.79132);
    }

    private double seconds;

    SpaceAge(double seconds) {
        this.seconds = seconds;
    }

    double getSeconds() {
        return seconds;
    }

    double onEarth() {
        return getSeconds() / SECONDS_PER_EARTH_YEAR;
    }

    double onMercury() {
        return onEarth() / RELATIVE_YEAR.get("Mercury");
    }

    double onVenus() {
        return onEarth() / RELATIVE_YEAR.get("Venus");
    }

    double onMars() {
        return onEarth() / RELATIVE_YEAR.get("Mars");
    }

    double onJupiter() {
        return onEarth() / RELATIVE_YEAR.get("Jupiter");
    }

    double onSaturn() {
        return onEarth() / RELATIVE_YEAR.get("Saturn");
    }

    double onUranus() {
        return onEarth() / RELATIVE_YEAR.get("Uranus");
    }

    double onNeptune() {
        return onEarth() / RELATIVE_YEAR.get("Neptune");
    }

}
