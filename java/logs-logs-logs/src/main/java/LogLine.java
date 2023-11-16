public class LogLine {
    private String line;

    public LogLine(String logLine) {
        this.line = logLine;
    }

    public LogLevel getLogLevel() {
        int idx = line.indexOf(']');
        String lvl = line.substring(1, idx);
        return LogLevel.getLevel(lvl);
    }

    public String getMessage() {
        int idx = line.indexOf(':');
        return line.substring(idx + 1).trim();
    }

    public String getOutputForShortLog() {
        LogLevel level = getLogLevel();
        return "%d:%s".formatted(level.getNumericLevel(), getMessage());
    }
}
