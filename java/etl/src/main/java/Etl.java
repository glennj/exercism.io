import java.util.AbstractMap;
// import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class Etl {

//    // Using an intermediary variable to hold results
//    Map<String, Integer> transform(Map<Integer, List<String>> old) {
//        Map<String, Integer> newMap = new HashMap<>();
//        old.forEach((num, list) -> list.forEach(str -> newMap.put(str.toLowerCase(), num)));
//        return newMap;
//    }

    // Purely stream-based
    Map<String, Integer> transform(Map<Integer, List<String>> old) {
        return old
                .entrySet()
                .stream()
                .flatMap(this::transformEntry)
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }

    private Stream<Map.Entry<String, Integer>> transformEntry(Map.Entry<Integer, List<String>> entry) {
        return entry
                .getValue()
                .stream()
                .map(str -> new AbstractMap.SimpleEntry<>(str.toLowerCase(), entry.getKey()));
    }
}
