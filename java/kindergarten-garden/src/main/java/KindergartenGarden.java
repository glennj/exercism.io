import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

class KindergartenGarden {

    private static final String[] STUDENTS = {
        "Alice", "Bob", "Charlie", "David", "Eve", "Fred",
        "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"
    )}

    private Map<String, List<Plant>> plots;
    private List<List<Character>> rows;

    KindergartenGarden(String garden) {
        this(garden, STUDENTS);
    }

    KindergartenGarden(String garden, String[] students) {
        assignPlots(garden, students);
    }

    private void assignPlots(String garden, String[] students) {
        plots = new HashMap<>();
        rows = parse(garden);

        for (int i = 0; i < rows.get(0).size() / 2; i++)
            plots.put(students[i], getPlants(i));
    }

    private List<List<Character>> parse(String garden) {
        return Arrays
                .stream(garden.split("\n"))
                .map(line -> line
                        .chars()
                        .mapToObj(c -> (char) c)
                        .collect(Collectors.toList())
                )
                .collect(Collectors.toList());
    }

    private List<Plant> getPlants(int idx) {
        return Arrays.asList(
                Plant.getPlant(rows.get(0).get(2 * idx)),
                Plant.getPlant(rows.get(0).get(2 * idx + 1)),
                Plant.getPlant(rows.get(1).get(2 * idx)),
                Plant.getPlant(rows.get(1).get(2 * idx + 1))
        );
    }
}
