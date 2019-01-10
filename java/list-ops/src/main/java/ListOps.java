import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;

class ListOps {

    static <T, U> U foldLeft(List<T> list, U initial, BiFunction<U, T, U> f) {
        U accumulator = initial;
        for (T element : list)
            accumulator = f.apply(accumulator, element);
        return accumulator;
    }

    // could be done with foldLeft, except
    // the function takes arguments in the opposite order.
    static <T, U> U foldRight(List<T> list, U initial, BiFunction<T, U, U> f) {
        U accumulator = initial;
        for (T element : reverse(list))
            accumulator = f.apply(element, accumulator);
        return accumulator;
    }

    // implement the rest using foldLeft

    static <T> List<T> append(List<T> list1, List<T> list2) {
//        return Stream.concat(list1.stream(), list2.stream()).collect(Collectors.toList());

        BiFunction<List<T>, T, List<T>> f = (acc, elem) -> {
            acc.add(elem);
            return acc;
        };
        // we need to be sure we can `add` to the initial list.
        // for example, Collections.emptyList() returns an _immutable_ list.
        List<T> initial = Collections.list(Collections.enumeration(list1));
        return foldLeft(list2, initial, f);
    }

    static <T> List<T> concat(List<List<T>> listOfLists) {
        return foldLeft(listOfLists, Collections.emptyList(), ListOps::append);
    }

    static <T> int size(List<T> list) {
        BiFunction<Integer, T, Integer> f = (count, elem) -> count + 1;
        return foldLeft(list, 0, f);
    }

    static <T> List<T> filter(List<T> list, Predicate<T> predicate) {
        BiFunction<List<T>, T, List<T>> f = (acc, elem) -> {
            if (predicate.test(elem))
                acc.add(elem);
            return acc;
        };
        return foldLeft(list, new ArrayList<>(), f);
    }

    static <T, U> List<U> map(List<T> list, Function<T, U> transform) {
        BiFunction<List<U>, T, List<U>> f = (acc, elem) -> {
            acc.add(transform.apply(elem));
            return acc;
        };
        return foldLeft(list, new ArrayList<>(), f);
    }

    static <T> List<T> reverse(List<T> list) {
        BiFunction<List<T>, T, List<T>> f = (acc, elem) -> {
            acc.add(0, elem);
            return acc;
        };
        return foldLeft(list, new ArrayList<>(), f);
    }

    private ListOps() {
        // No instances.
    }

}
