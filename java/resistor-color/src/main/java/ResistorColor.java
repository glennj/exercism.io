import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;

class ResistorColor {
    private static final Map<String,Integer> COLOR_MAP;
    static {
        COLOR_MAP = new HashMap<>();
        COLOR_MAP.put("black", 0);
        COLOR_MAP.put("brown", 1);
        COLOR_MAP.put("red", 2);
        COLOR_MAP.put("orange", 3);
        COLOR_MAP.put("yellow", 4);
        COLOR_MAP.put("green", 5);
        COLOR_MAP.put("blue", 6);
        COLOR_MAP.put("violet", 7);
        COLOR_MAP.put("grey", 8);
        COLOR_MAP.put("white", 9);
    }

    // What if color is not in map? Results are unspecified...
    int colorCode(String color) {
        return COLOR_MAP.get(color);
    }

    // Stream based. Ensure colors are sorted by code ascending.
    String[] colors() {
        return COLOR_MAP
                .keySet()
                .stream()
                .sorted(Comparator.comparingInt(this::colorCode))
                .toArray(String[]::new);
    }
}
