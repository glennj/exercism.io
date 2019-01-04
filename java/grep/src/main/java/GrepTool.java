import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class GrepTool {

    private boolean optLineNumbers;    // -n option
    private boolean optIgnoreCase;     // -i
    private boolean optInverse;        // -v
    private boolean optMatchLine;      // -x
    private boolean optOnlyFilenames;  // -l
    private boolean optWithFilenames;  // if multiple files specified

    // A container to hold data about each matched line.
    private static class LineData {
        private String filename;
        private int lineNumber;
        private String line;
        LineData(String filename, int lineNumber, String line) {
            this.filename = filename;
            this.lineNumber = lineNumber;
            this.line = line;
        }
        String getFilename() { return filename; }
        String getLineNumberString() { return Integer.toString(lineNumber); }
        String getLine() { return line; }
    }

    public String grep(String pattern, List<String> options, List<String> filenames) {
        parseOptions(options, filenames.size());
        Pattern p = getPattern(pattern);

        Stream<LineData> matches = filenames
                .stream()
                .flatMap(this::fileLines)
                .filter(data -> p.matcher(data.getLine()).matches());

        return (optOnlyFilenames
                    ? matches.map(LineData::getFilename).distinct()
                    : matches.map(this::formatOutput)
        ).collect(Collectors.joining("\n"));
    }

    private void parseOptions(List<String> options, int numFiles) {
        optIgnoreCase    = options.contains("-i");
        optLineNumbers   = options.contains("-n");
        optOnlyFilenames = options.contains("-l");
        optMatchLine     = options.contains("-x");
        optInverse       = options.contains("-v");
        optWithFilenames = numFiles > 1;
    }

    private Pattern getPattern(String pattern) {
        String patt = ".*" + pattern + ".*";
        if (optMatchLine) patt = pattern;
        if (optInverse)   patt = "^(?!.*" + pattern + ").*";

        int flags = 0;
        if (optIgnoreCase) flags |= Pattern.CASE_INSENSITIVE;

        return Pattern.compile(patt, flags);
    }

    private Stream<LineData> fileLines(String filename) {
        Stream<LineData> stream = Stream.empty();
        AtomicInteger lineNumber = new AtomicInteger();

        try {
            stream = Files
                    .lines(Paths.get(filename))
                    .map(line -> new LineData(filename, lineNumber.incrementAndGet(), line));
        }
        catch (IOException e) {
            e.printStackTrace();
        }

        return stream;
    }

    private String formatOutput(LineData lineData) {
        List<String> parts = new ArrayList<>();

        if (optWithFilenames) parts.add(lineData.getFilename());
        if (optLineNumbers)   parts.add(lineData.getLineNumberString());
        parts.add(lineData.getLine());

        return String.join(":", parts);
    }
}
