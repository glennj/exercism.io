import java.util.ArrayList;
import java.util.List;

public class LanguageList {
    private final List<String> langList = new ArrayList<>();

    public boolean isEmpty() {
        return langList.isEmpty();
    }

    public void addLanguage(String language) {
        langList.add(language);
    }

    public void removeLanguage(String language) {
        langList.remove(language);
    }

    public String firstLanguage() {
        return langList.get(0);
    }

    public int count() {
        return langList.size();
    }

    public boolean containsLanguage(String language) {
        return langList.contains(language);
    }

    public boolean isExciting() {
        return containsLanguage("Java") || containsLanguage("Kotlin");
    }
}
