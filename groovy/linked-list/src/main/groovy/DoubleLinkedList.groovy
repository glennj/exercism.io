class DoubleLinkedList<T> {

    private Node<T> head
    private Node<T> tail

    void push(T value) {
        def node = new Node(value)
        if (tail) {
            node.prev = tail
            tail.next = node
        }
        else {
            head = node
        }
        tail = node
    }

    T pop() {
        if (tail) {
            def node = tail
            if (node.prev) {
                node.prev.next = null
                tail = node.prev
            }
            else {
                head = null
                tail = null
            }
            node.value
        }
    }

    void unshift(T value) {
        def node = new Node(value)
        if (head) {
            node.next = head
            head.prev = node
        }
        else {
            tail = node
        }
        head = node
    }

    T shift() {
        if (head) {
            def node = head
            if (node.next) {
                node.next.prev = null
                head = node.next
            }
            else {
                head = null
                tail = null
            }
            node.value
        }
    }
}

class Node<T> {
    final T value
    Node<T> next
    Node<T> prev
    Node(T value) {
        this.value = value
    }
}
