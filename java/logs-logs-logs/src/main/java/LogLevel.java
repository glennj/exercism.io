public enum LogLevel {
    TRACE(  "TRC", 1),
    DEBUG(  "DBG", 2),
    INFO(   "INF", 4),
    WARNING("WRN", 5),
    ERROR(  "ERR", 6),
    FATAL(  "FTL", 42),
    UNKNOWN("UNK", 0);

    private final String abbr;
    private final int num;

    LogLevel(String abbreviation, int numericLevel) {
        abbr = abbreviation;
        num = numericLevel;
    }

    public String getAbbreviation() {
        return abbr;
    }

    public int getNumericLevel() {
        return num;
    }

    public static LogLevel getLevel(String abbreviation) {
        for (LogLevel level : values())
            if (level.getAbbreviation().equals(abbreviation))
                return level;
        return UNKNOWN;
    }
}
