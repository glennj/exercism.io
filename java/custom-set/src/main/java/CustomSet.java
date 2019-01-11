import java.util.Collection;
import java.util.Set;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class CustomSet<T> implements Set<T> {

    // using a List as the backend storage
    private List<T> elements = new ArrayList<>();

    CustomSet(Collection<T> elements) {
        addAll(elements);
    }

    // delegating Collection abstract methods to the storage
    public boolean contains(Object element) { return elements.contains(element); }
    public int size() { return elements.size(); }
    public boolean isEmpty() { return elements.isEmpty(); }
    public boolean remove(Object o) { return elements.remove(o); }
    public boolean containsAll(Collection<?> c) { return elements.containsAll(c); }
    public boolean removeAll(Collection<?> c) { return elements.removeAll(c); }
    public boolean retainAll(Collection<?> c) { return elements.retainAll(c); }
    public void clear() { elements.clear(); }
    public Iterator<T> iterator() { return elements.iterator(); }
    public Object[] toArray() { return elements.toArray(); }
    public <T> T[] toArray(T[] a) { return elements.toArray(a); }

    // returns true if the element was added.
    public boolean add(T element) {
        if (elements.contains(element))
            return false;
        return elements.add(element);
    }

    // returns true if any element was added.
    public boolean addAll(Collection<? extends T> c) {
        boolean added = false;
        for (T element : c)
            added = add(element) || added;
        return added;
    }

    // test if 'other' is a subset of 'this'
    boolean isSubset(CustomSet<T> other) {
        return containsAll(other);
    }

    boolean isDisjoint(CustomSet<T> other) {
        return getIntersection(other).isEmpty();
    }

    @Override
    public boolean equals(Object o) {
        if (o == null) return false;
        if (o == this) return true;
        if (!(o instanceof CustomSet)) return false;
        @SuppressWarnings("unchecked")
        CustomSet<T> other = (CustomSet<T>) o;
        return size() == other.size() && isSubset(other);
    }

    @Override public int hashCode() {
        return elements.hashCode();
    }

    // return my elements that are in 'other'
    CustomSet<T> getIntersection(CustomSet<T> other) {
        return new CustomSet<>(
                elements.stream()
                        .filter(other::contains)
                        .collect(Collectors.toList())
        );
    }

    // return my elements that are not in 'other'
    CustomSet<T> getDifference(CustomSet<T> other) {
        return new CustomSet<>(
                elements.stream()
                        .filter(elem -> !other.contains(elem))
                        .collect(Collectors.toList())
        );
    }

    CustomSet<T> getUnion(CustomSet<T> other) {
        return new CustomSet<>(
                Stream.concat(
                        other.stream(),
                        getDifference(other).stream()
                )
                .collect(Collectors.toList())
        );
    }
}
