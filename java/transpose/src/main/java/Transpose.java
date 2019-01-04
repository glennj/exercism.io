import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Transpose {
    public String transpose(String input) {
        String[] lines = pad(input.split("\n"));
        int max = lines[0].length();

        List<String> result = new ArrayList<>();
        for (int i = 0; i < max; i++) {
            StringBuilder sb = new StringBuilder();
            for (String line : lines) {
                if (i >= line.length())
                    break;
                sb.append(line.charAt(i));
            }
            result.add(sb.toString());
        }
        return String.join("\n", result);
    }

    /*
     * Pad an array of lines such that each line
     * is at least as long as any following lines.
     *
     * example: ["foo", "bang", "bar-baz", "qux"]
     * becomes: ["foo    ", "bang   ", "bar-baz", "qux"]
     * because line 2 is longer
     */
    private String[] pad(String[] lines) {
        int max = lines[lines.length - 1].length();
        for (int i = lines.length - 2; i >= 0; i--) {
            int len = lines[i].length();
            if (len < max)
                lines[i] = StringUtils.rightPad(lines[i], max);
            else
                max = len;
        }
        return lines;
    }

    private static class StringUtils {
        private static String rightPad(String s, int size) {
            return rightPad(s, size, ' ');
        }

        private static String rightPad(String s, int size, char padChar) {
            if (s.length() > size) return s;
            char[] spaces = new char[size];
            Arrays.fill(spaces, padChar);
            StringBuilder sb = new StringBuilder(s).append(spaces);
            return sb.substring(0, size);
        }

        private static String leftPad(String s, int size) {
            return rightPad(s, size, ' ');
        }

        private static String leftPad(String s, int size, char padChar) {
            if (s.length() > size) return s;
            char[] spaces = new char[size];
            Arrays.fill(spaces, padChar);
            StringBuilder sb = new StringBuilder(s).insert(0, spaces);
            return sb.substring(sb.length() - size);
        }
    }
}
