import java.util.regex.*;

class Markdown {
    private static final Pattern HEADER    = Pattern.compile("^(#+)\\s+(.*)");
    private static final Pattern LIST_ITEM = Pattern.compile("^\\*\\s+(.*)");
    private static final Pattern STRONG    = Pattern.compile("\\b__(\\p{Alnum}(?:.*\\p{Alnum})?)__\\b");
    private static final Pattern EMPH      = Pattern.compile("\\b_(\\p{Alnum}(?:.*\\p{Alnum})?)_\\b");

    private boolean inList;

    String parse(String markdown) {
        StringBuilder html = new StringBuilder();
        inList = false;

        for (String line : markdown.split("\n")) {
            if (parseHeader(line, html))   continue;
            if (parseListItem(line, html)) continue;
            parseParagraph(line, html);
        }

        if (inList)
            html.append("</ul>");

        return html.toString();
    }

    private boolean parseHeader(String markdown, StringBuilder html) {
        // pattern is anchored at start of line, so either matches or not
        Matcher m = HEADER.matcher(markdown);
        boolean result = m.find();
        if (result) {
            int level = m.group(1).length();
            html.append(String.format("<h%d>%s</h%d>", level, m.group(2), level));
        }
        return result;
    }

    private boolean parseListItem(String markdown, StringBuilder html) {
        // pattern is anchored at start of line, so either matches or not
        Matcher m = LIST_ITEM.matcher(markdown);
        boolean result = m.find();
        if (result) {
            html.append(inList ? "" : "<ul>")
                .append("<li>")
                .append(handleMarkup(m.group(1)))
                .append("</li>");
            inList = true;
        }
        else if (inList) {
            html.append("</ul>");
            inList = false;
        }
        return result;
    }

    private void parseParagraph(String markdown, StringBuilder html) {
        html.append("<p>")
            .append(handleMarkup(markdown))
            .append("</p>");
    }

    private String handleMarkup(String markdown) {
        String markedUp;
        markedUp = STRONG.matcher(markdown).replaceAll("<strong>$1</strong>");
        markedUp = EMPH.matcher(markedUp).replaceAll("<em>$1</em>");
        return markedUp;
    }
}