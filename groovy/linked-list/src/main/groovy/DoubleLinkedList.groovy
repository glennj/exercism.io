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

    void forEach(closure) {
        def node = head
        while (node != null) {
            closure.call(node)
            node = node.next
        }
    }

    int count() {
        def c = 0
        forEach { c++ }
        c
    }

    void delete(T value) {
        def node = head
        while (node != null) {
            if (node.value != value) {
                node = node.next
                continue
            }

            if (node.next) {
                node.next.prev = node.prev
            }
            else {
                tail = node.prev
            }
            if (node.prev) {
                node.prev.next = node.next
            }
            else {
                head = node.next
            }
            break
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
