module binary_search;

import std.conv : to;
import std.exception : enforce;

class BinarySearch {
    private immutable int[] values;

    this(immutable int[] values) {
        this.values = values;
    }

    final int find(immutable int value) => findR(value, 0, values.length - 1);

    private int findR(immutable int value, size_t left, size_t right) {
        // note that `mid - 1` when `mid == 0` will not be negative.
        enforce(right != size_t.max && left <= right, "value not found");

        size_t mid = (left + right) / 2;
        if (value < values[mid]) return findR(value, left, mid - 1);
        if (value > values[mid]) return findR(value, mid + 1, right);
        return mid.to!int;
    }
}

unittest
{
    const int allTestsEnabled = 1;

    // Finds a value in an array with one element
    {
        BinarySearch bs = new BinarySearch([6]);
        assert(bs.find(6) == 0);
    }

    static if (allTestsEnabled)
    {

        import std.exception : assertThrown;

        // Finds a value in the middle of an array
        {
            BinarySearch bs = new BinarySearch([1, 3, 4, 6, 8, 9, 11]);
            assert(bs.find(6) == 3);
        }

        // Finds a value at the beginning of an array
        {
            BinarySearch bs = new BinarySearch([1, 3, 4, 6, 8, 9, 11]);
            assert(bs.find(1) == 0);
        }

        // Finds a value at the end of an array
        {
            BinarySearch bs = new BinarySearch([1, 3, 4, 6, 8, 9, 11]);
            assert(bs.find(11) == 6);
        }

        // Finds a value in an array of odd length
        {
            BinarySearch bs = new BinarySearch([1, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 634]);
            assert(bs.find(144) == 9);
        }

        // Finds a value in an array of even length
        {
            BinarySearch bs = new BinarySearch([1, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]);
            assert(bs.find(21) == 5);
        }

        // Identifies that a value is not included in the array
        {
            BinarySearch bs = new BinarySearch([1, 3, 4, 6, 8, 9, 11]);
            assertThrown(bs.find(7));
        }

        // A value smaller than the array's smallest value is not found
        {
            BinarySearch bs = new BinarySearch([1, 3, 4, 6, 8, 9, 11]);
            assertThrown(bs.find(0));
        }

        // A value larger than the array's largest value is not found
        {
            BinarySearch bs = new BinarySearch([1, 3, 4, 6, 8, 9, 11]);
            assertThrown(bs.find(13));
        }

        // Nothing is found in an empty array
        {
            BinarySearch bs = new BinarySearch([]);
            assertThrown(bs.find(1));
        }

        // Nothing is found when the left and right bounds cross
        {
            BinarySearch bs = new BinarySearch([1, 2]);
            assertThrown(bs.find(0));
        }

    }
}
