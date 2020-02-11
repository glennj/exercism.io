object BinarySearch {
    tailrec fun search(list: List<Int>, item: Int): Int {
        if (list.isEmpty())
            throw NoSuchElementException()

        var left = 0
        var right = list.size - 1
        val mid = right / 2

        when {
            item == list[mid] -> return mid
            item  < list[mid] -> right = mid - 1
            else              -> left = mid + 1
        }

        return left + search(list.subList(left, right + 1), item)
    }
}
