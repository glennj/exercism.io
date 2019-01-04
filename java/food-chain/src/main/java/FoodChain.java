import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class FoodChain {

    private static class Animal {
        private String name;
        private String tag = "";
        private String extra = "";
        private boolean apexPredator = false;
        // constructor
        Animal(String name) { this.name = name; }
        // getters
        String getName()   { return name; }
        String getTag()    { return tag; }
        String getExtra()  { return extra; }
        boolean isKiller() { return apexPredator; }
        //setters
        Animal setExtra(String extra) { this.extra = extra;  return this; }
        Animal setTag(String tag)     { this.tag = tag;      return this; }
        Animal killer()               { apexPredator = true; return this; }
    }

    private static final List<Animal> CHAIN = new ArrayList<>();
    static {
        CHAIN.add(new Animal("fly"));
        CHAIN.add(new Animal("spider")
                .setExtra(" that wriggled and jiggled and tickled inside her")
                .setTag("It wriggled and jiggled and tickled inside her."));
        CHAIN.add(new Animal("bird").setTag("How absurd to swallow a bird!"));
        CHAIN.add(new Animal("cat").setTag("Imagine that, to swallow a cat!"));
        CHAIN.add(new Animal("dog").setTag("What a hog, to swallow a dog!"));
        CHAIN.add(new Animal("goat").setTag("Just opened her throat and swallowed a goat!"));
        CHAIN.add(new Animal("cow").setTag("I don't know how she swallowed a cow!"));
        CHAIN.add(new Animal("horse").setTag("She's dead, of course!").killer());
    }

    public String song() {
        return verses(1, CHAIN.size());
    }

    public String verses(int start, int end) {
        return IntStream
                .rangeClosed(start, end)
                .mapToObj(this::verse)
                .collect(Collectors.joining("\n\n"));
    }

    public String verse(int num) throws IllegalArgumentException {
        if (num < 1 || num > CHAIN.size())
            throw new IllegalArgumentException();

        Animal animal = CHAIN.get(num - 1);

        return Stream.concat(
                Stream.of(iKnow(animal)),
                animal.isKiller()
                    ? Stream.empty()
                    : Stream.concat(
                            IntStream
                                    .rangeClosed(1, num - 1)
                                    .boxed()
                                    .sorted(Collections.reverseOrder())
                                    .map(this::hunt),
                            Stream.of(iDontKnow())
                    )
        )
        .collect(Collectors.joining());
    }

    private String iKnow(Animal animal) {
        String tag = animal.getTag();
        if (!tag.isEmpty() && !animal.isKiller()) tag += "\n";
        return String.format("I know an old lady who swallowed a %s.\n%s",
                animal.getName(), tag);
    }

    private String iDontKnow() {
        return "I don't know why she swallowed the fly. Perhaps she'll die.";
    }

    private String hunt(int i) {
        Animal predator = CHAIN.get(i);
        Animal prey = CHAIN.get(i - 1);
        return String.format("She swallowed the %s to catch the %s%s.\n",
                predator.getName(), prey.getName(), prey.getExtra());
    }
}
