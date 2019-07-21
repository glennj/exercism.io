class BinarySearch {

    List data

    BinarySearch(List data) {
        this.data = data
    }

    int indexOf(item) {
        def a = 0
        def b = data.size() - 1
        while (a <= b) {
            def mid = (a + b).intdiv(2)
            if (item == data[mid])
                return mid
            else if (item < data[mid])
                b = mid - 1
            else
                a = mid + 1
        }
        return -1
    }
}
