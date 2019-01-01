//import java.util.Set;
//import java.util.stream.Collectors;

class IsogramChecker {

    boolean isIsogram(String phrase) {
        String alphaOnly = phrase
                .toLowerCase()
                .replaceAll("[\\s-]", "");

//        Set<Character> uniq = alphaOnly
//                .chars()
//                .mapToObj(c -> new Character((char) c))
//                .collect(Collectors.toSet());
//        return uniq.size() == alphaOnly.length();

        int distinct = (int) alphaOnly.chars().distinct().count();
        return distinct == alphaOnly.length();
    }
}
