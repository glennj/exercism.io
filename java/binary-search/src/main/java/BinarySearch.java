import java.util.List;

public class BinarySearch<T extends Comparable> {
    private final List<T> data;

    BinarySearch(List<T> input) {
        if (!isSorted(input))
            throw new IllegalArgumentException("List must be sorted");
        data = input;
    }

    int indexOf(T datum) {
        int i = 0;
        int j = data.size() - 1;
        while (i <= j) {
            int mid = i + (int)Math.ceil((j - i) / 2.0);
            T midValue = data.get(mid);
            switch (datum.compareTo(midValue)) {
                case -1:
                    j = mid - 1;
                    break;
                case 1:
                    i = mid + 1;
                    break;
                default:
                    return mid;
            }
        }
        // not found
        return -1;
    }

    private boolean isSorted(List<T> list) {
        if (!list.isEmpty()) {
            T prev = list.get(0);
            for (int i = 1; i < list.size(); i++) {
                T current = list.get(i);
                if (prev.compareTo(current) == 1) return false;
                prev = current;
            }
        }
        return true;
    }
}
