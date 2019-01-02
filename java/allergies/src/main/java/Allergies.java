import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Allergies {
    private int score;
    private final List<Allergen> allergens;

    Allergies(int score) {
        this.score = score;
        allergens = Stream.of(Allergen.values())
                .filter(allergen -> (score & allergen.getScore()) != 0)
                .collect(Collectors.toList());
    }

    boolean isAllergicTo(Allergen allergen) {
        return allergens.contains(allergen);
    }

    List<Allergen> getList() {
        return allergens;
    }
}
