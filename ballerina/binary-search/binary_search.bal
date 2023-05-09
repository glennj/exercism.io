# Search an array for a value and return the index.
#
# + array - a sorted array of integers
# + value - the integer item to find
# + return - the index of the value, or nil if the value is not found
public function find(int[] array, int value) returns int? {
    return findRec(array, value);
}

function findRec(int[] array, int value, int offset = 0) returns int? {
    if array.length() == 0 {
        return ();
    }

    int mid = array.length() / 2;

    if value == array[mid] {
        return offset + mid;
    } else if value < array[mid] {
        return findRec(array.slice(0, mid), value, offset);
    } else {
        return findRec(array.slice(mid + 1, array.length()), value, offset + mid + 1);
    }
}
