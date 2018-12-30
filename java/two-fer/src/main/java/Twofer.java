import java.text.MessageFormat;
import java.util.Optional;

class Twofer {
    String twofer(String name) {
        // name = name == null ? "you" : name;
        // return "One for " + name + ", one for me";

        // StringBuilder sb = new StringBuilder("One for ");
        // sb.append(name == null ? "you" : name);
        // sb.append(", one for me.");
        // return sb.toString();

        // return String.format(
        //     "One for %s, one for me.",
        //     name == null ? "you" : name
        // );

        // return MessageFormat.format(
        //     "One for {0}, one for me.",
        //     name == null ? "you" : name
        // );

        return MessageFormat.format(
            "One for {0}, one for me.",
            Optional.ofNullable(name).orElse("you")
        );
    }
}
