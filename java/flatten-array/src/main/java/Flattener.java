import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class Flattener {
    List<Object> flatten(List<Object> list) {
//        return flattenIterative(list);
        return flattenStreamed(list);
    }

    // courtesy of lianphearin
    // https://exercism.io/tracks/java/exercises/flatten-array/solutions/607a56269b5f49dca160df89ff9cb812
    @SuppressWarnings("unchecked")
    private List<Object> flattenStreamed(List<Object> list) {
        return list
                .stream()
                .filter(Objects::nonNull)
                .flatMap(obj ->
                    obj instanceof List
                        ? flattenStreamed((List<Object>) obj).stream()
                        : Stream.of(obj)
                )
                .collect(Collectors.toList());
    }

    @SuppressWarnings("unchecked")
    private List<Object> flattenIterative(List<Object> list) {
        List<Object> result = new ArrayList<>();

        for (Object obj : list) {
            if (obj instanceof List)
                result.addAll(flattenIterative((List<Object>) obj));
            else if (Objects.nonNull(obj))
                result.add(obj);
        }

        return result;
    }
}
