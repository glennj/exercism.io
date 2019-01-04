import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Collections;
import java.util.stream.Collectors;

public class School {
    private Map<Integer, List<String>> roster = new HashMap<>();

    // Return the list of students in the given grade,
    // initializing the list if absent from the map.
    public List<String> grade(int grade) {
        return roster.computeIfAbsent(grade, ArrayList::new);
    }

    public void add(String student, int gradeNum) {
        grade(gradeNum).add(student);
        Collections.sort(grade(gradeNum));
    }

    public List<String> roster() {
        return roster
                .keySet()
                .stream()
                .sorted()
                .flatMap(gradeNum -> grade(gradeNum).stream())
                .collect(Collectors.toList());
    }
}
