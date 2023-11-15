public class LogLevels {
    
    public static String message(String logLine) {
        int i = logLine.indexOf(':');
		return logLine.substring(i + 1).trim();
    }

    public static String logLevel(String logLine) {
		/*
		return logLine.substring(
				logLine.indexOf('[') + 1,
				logLine.indexOf(']')
		).toLowerCase();
		*/
		/*
		String[] pieces = logLine.split("[\\[\\]]", 3);
		return pieces[1].toLowerCase();
		*/
		return logLine.split("[\\[\\]]", 3)[1].toLowerCase();
    }

    public static String reformat(String logLine) {
        // return message(logLine) + " (" + logLevel(logLine) + ")";
		/*
		return new StringBuilder(logLine.length())
				.append(message(logLine))
				.append(" (")
				.append(logLevel(logLine))
				.append(")")
				.toString();
		*/
		//return String.format("%s (%s)", message(logLine), logLevel(logLine));

		return "%s (%s)".formatted(
				message(logLine),
				logLevel(logLine)
		);
    }
}
