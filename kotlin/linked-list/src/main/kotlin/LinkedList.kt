
class Deque<T> {

    private data class Node<T>(val value: T, var next: Node<T>? = null, var prev: Node<T>? = null)

    private var head: Node<T>? = null
    private var tail: Node<T>? = null

    fun push(value: T) {
        tail.let { oldTail ->
            tail = Node(value, prev = oldTail)
            if (oldTail == null) {
                head = tail
            } else {
                oldTail.next = tail
                tail!!.prev = oldTail
            }
        }
    }

    fun unshift(value: T) {
        head.let { oldHead ->
            head = Node(value, next = oldHead)
            if (oldHead == null) {
                tail = head
            } else {
                oldHead.prev = head
                head!!.next = oldHead
            }
        }
    }

    fun pop(): T? {
        var value: T? = null
        tail?.let { oldTail ->
            if (oldTail.prev == null) {
                head = null
                tail = null
            } else {
                tail = oldTail.prev
                tail!!.next = null
            }
            value = oldTail.value
        }
        return value
    }

    fun shift(): T? {
        var value: T? = null
        head?.let { oldHead ->
            if (oldHead.next == null) {
                head = null
                tail = null
            } else {
                head = oldHead.next
                head!!.prev = null
            }
            value = oldHead.value
        }
        return value
    }
}

