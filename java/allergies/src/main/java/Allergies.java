import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Allergies {
    private int score;

    Allergies(int score) {
        this.score = score;
    }

    boolean isAllergicTo(Allergen allergen) {
        return (score & allergen.getScore()) != 0;
    }

    List<Allergen> getList() {
        return Stream.of(Allergen.values())
                .filter(this::isAllergicTo)
                .collect(Collectors.toList());
    }
}
