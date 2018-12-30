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

/* community

https://exercism.io/tracks/java/exercises/space-age/solutions/3c3ef549276a434e8fa96b25eaf61a9d

using an enum:

    private enum Planet {
        EARTH(1.0),
        MERCURY(0.2408467),
        VENUS(0.61519726),
        MARS(1.8808158),
        JUPITER(11.862615),
        SATURN(29.447498),
        URANUS(84.016846),
        NEPTUNE(164.79132);

        private final double orbitalPeriodInEarthYears;

        Planet(double orbitalPeriodInEarthYears) {
            this.orbitalPeriodInEarthYears = orbitalPeriodInEarthYears;
        }

        public double getOrbitalPeriodInEarthYears() {
            return orbitalPeriodInEarthYears;
        }
    }
    // ...
    public double onEarth() {
        return getOrbitalAge(Planet.EARTH);
    }
    // ...
    private double getOrbitalAge(Planet planet) {
        return earthYears / planet.getOrbitalPeriodInEarthYears();
    }
*/