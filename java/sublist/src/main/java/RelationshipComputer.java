import java.lang.reflect.Field;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/* I will transform the input lists into strings, and see if the strings
 * are equal or are substrings contained within each other.
 *
 * The separator character is the crux: can we be sure that the separator
 * character does not exist in the input data. I'm choosing the ASCII
 * "Field Separator" character, 0x1f.
 *
 * An improvement could be to examine all the stringified input data and
 * then select a character that we can observe is not present.
 *
 * I'm also assuming that `String::valueOf` is sufficient to stringify the data.
 */

class RelationshipComputer<T> {
    private static final String FIELD_SEP = String.valueOf((char) 0x1f);

    Relationship computeRelationship(List<T> a, List<T> b) {
        String aString = stringify(a);
        String bString = stringify(b);

        if (aString.equals(bString))   return Relationship.EQUAL;
        if (aString.contains(bString)) return Relationship.SUPERLIST;
        if (bString.contains(aString)) return Relationship.SUBLIST;
        return Relationship.UNEQUAL;
    }

    private String stringify(List<T> list) {
        if (list.isEmpty()) return "";

        List<String> elements = list
                .stream()
                .map(String::valueOf)
                .collect(Collectors.toList());

        /* Add empty strings to the start and end of the list.
         * This is to add the FIELD_SEP char to the start and end of the stringification
         * to avoid this scenario
         *     A: list(10, 10)
         *     B: list(0, 1)
         * Result should be UNEQUAL.
         *
         * Without the extra elements, stringification would look like
         *     A: "10,10"
         *     B:  "0,1"
         * and string A contains string B, and that will result in SUPERLIST
         *
         * With the extra elements, stringification will look like
         *     A: ",10,10,"
         *     B: ",0,1,"
         * resulting in the desired relationship.
         */
        elements.add(0, "");
        elements.add("");

        return String.join(FIELD_SEP, elements);
    }
}
