import java.util.List;
import java.util.Set;
import java.util.HashSet;

class GottaSnatchEmAll {

    static Set<String> newCollection(List<String> cards) {
        return new HashSet<String>(cards);
    }

    static boolean addCard(String card, Set<String> collection) {
        return collection.add(card);
    }

    static boolean canTrade(Set<String> myCollection, Set<String> theirCollection) {
        return !myCollection.containsAll(theirCollection) && !theirCollection.containsAll(myCollection);
    }

    static Set<String> commonCards(List<Set<String>> collections) {
        Set<String> common = new HashSet<>(collections.get(0));
        for (int i = 1; i < collections.size(); i++) 
            common.retainAll(collections.get(i));
        return common;
    }

    static Set<String> allCards(List<Set<String>> collections) {
        Set<String> union = new HashSet<>(collections.get(0));
        for (int i = 1; i < collections.size(); i++) 
            union.addAll(collections.get(i));
        return union;
    }
}
