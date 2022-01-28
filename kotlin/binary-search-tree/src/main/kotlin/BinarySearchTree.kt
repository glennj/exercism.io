class BinarySearchTree<T : Comparable<T>> {

    data class Node<T : Comparable<T>>(
            val data: T,
            var left: Node<T>? = null,
            var right: Node<T>? = null
    ) {
        fun insert(data: T): Node<T> {
            when {
                data <= this.data -> left = left?.insert(data) ?: Node(data)
                else -> right = right?.insert(data) ?: Node(data)
            }
            return this
        }

        fun asSortedList(): List<T> =
                (left?.asSortedList() ?: emptyList()) +
                listOf(data) +
                (right?.asSortedList() ?: emptyList())

        fun asLevelOrderList(height: Int): List<T> =
                (1..height).flatMap { givenLevel(it) }

        private fun givenLevel(level: Int): List<T> =
            when(level) {
                1 -> listOf(data)
                else -> {
                    (left?.givenLevel(level - 1) ?: emptyList()) +
                    (right?.givenLevel(level - 1) ?: emptyList())
                }
            }
    }

    var root: Node<T>? = null

    fun insert(value: T) {
        root = root?.insert(value) ?: Node(value)
    }

    fun asSortedList(): List<T> = root?.asSortedList() ?: emptyList()

    fun asLevelOrderList(): List<T> = root?.asLevelOrderList(height()) ?: emptyList()

    fun height(node: Node<T>? = root): Int =
        when(node) {
            null -> 0
            else -> listOf(height(node.left), height(node.right)).max()!! + 1
        }
}
