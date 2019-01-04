import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class House {

    private static class Actor {
        private String name;
        private String action;
        Actor(String name) { this.name = name; }
        String getName() { return name; }
        String getAction() { return action; }
        Actor setAction(String action) { this.action = action; return this; }
    }

    private static List<Actor> Actors = new ArrayList<>();
    static {
        Actors.add(new Actor("house that Jack built."));
        Actors.add(new Actor("malt").setAction("lay in"));
        Actors.add(new Actor("rat").setAction("ate"));
        Actors.add(new Actor("cat").setAction("killed"));
        Actors.add(new Actor("dog").setAction("worried"));
        Actors.add(new Actor("cow with the crumpled horn").setAction("tossed"));
        Actors.add(new Actor("maiden all forlorn").setAction("milked"));
        Actors.add(new Actor("man all tattered and torn").setAction("kissed"));
        Actors.add(new Actor("priest all shaven and shorn").setAction("married"));
        Actors.add(new Actor("rooster that crowed in the morn").setAction("woke"));
        Actors.add(new Actor("farmer sowing his corn").setAction("kept"));
        Actors.add(new Actor("horse and the hound and the horn").setAction("belonged to"));
    }

    public String sing() {
        return verses(1, Actors.size());
    }

    public String verses(int start, int end) {
        return IntStream
                .rangeClosed(start, end)
                .mapToObj(this::verse)
                .collect(Collectors.joining("\n"));
    }

    public String verse(int num) {
        if (num < 1 || num > Actors.size())
            throw new IllegalArgumentException();

        return Stream.concat(
                Stream.of(thisClause(num - 1)),
                IntStream
                        .rangeClosed(1, num - 1)
                        .boxed()
                        .sorted(Collections.reverseOrder())
                        .map(this::thatClause)
        )
        .collect(Collectors.joining(" "));
    }

    private String thisClause(int i) {
        String actor = Actors.get(i).getName();
        return String.format("This is the %s", actor);
    }

    private String thatClause(int i) {
        String action = Actors.get(i).getAction();
        String target = Actors.get(i - 1).getName();
        return String.format("that %s the %s", action, target);
    }
}
