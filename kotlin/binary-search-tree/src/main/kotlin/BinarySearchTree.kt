class BinarySearchTree<T : Comparable<T>> {

    data class Node<T>(val data: T, val left: T?, val right: T?)

    var root: Node<T>? = null

    fun insert(value: T) {
        val node = Node(T)
        if (this == null)
            root = node
        else
            if ()
    }

    fun asSortedList(): List<T> {
        TODO("Delete this statement and write your own implementation.")
    }

    fun asLevelOrderList(): List<T> {
        TODO("Delete this statement and write your own implementation.")
    }

}
