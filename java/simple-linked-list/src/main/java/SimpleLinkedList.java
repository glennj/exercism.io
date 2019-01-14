import java.lang.reflect.Array;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

class SimpleLinkedList<T> {
    private static class Node<T> {
        private final T datum;
        private Node<T> next;
        Node(T datum) { this.datum = datum; }
        T getDatum() { return datum; }
        Node<T> getNext() { return next; }
        void setNext(Node<T> node) { next = node; }
    }

    private Node<T> head;
    private int size = 0;

    SimpleLinkedList() {}

    SimpleLinkedList(T[] values) {
        for (T value : values) {
            push(value);
        }
    }

    SimpleLinkedList(List<T> values) {
        for (T value : values) {
            push(value);
        }
    }

    void push(T value) {
        Node<T> node = new Node(value);
        if (!isEmpty()) {
            node.setNext(head);
        }
        head = node;
        size++;
    }

    T pop() throws NoSuchElementException {
        if (isEmpty())
            throw new NoSuchElementException();

        T datum = head.getDatum();
        head = head.getNext();
        size--;
        return datum;
    }

    int size() {
//        // Could implement without an instance variable using `forEach`
//        AtomicInteger count = new AtomicInteger(0);
//        forEach(n -> count.incrementAndGet());
//        return count.get();
        return size;
    }

    boolean isEmpty() {
        return size == 0;
    }

    void forEach(Consumer<Node<T>> func) {
        Node<T> current = head;
        while (current != null) {
            func.accept(current);
            current = current.getNext();
        }
    }

    T[] asArray(Class<T> c) {
        // https://stackoverflow.com/questions/529085/how-to-create-a-generic-array-in-java
        @SuppressWarnings("unchecked")
        final T[] result = (T[]) Array.newInstance(c, size());
        AtomicInteger i = new AtomicInteger(0);
        forEach(node -> result[i.getAndIncrement()] = node.getDatum());
        return result;
    }

    void reverse() {
        SimpleLinkedList<T> reversed = new SimpleLinkedList<>();
        while (!isEmpty())
            reversed.push(this.pop());
        head = reversed.head;
        size = reversed.size;
        reversed.head = null;
    }
}
