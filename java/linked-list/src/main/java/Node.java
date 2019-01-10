import java.util.Optional;

public class Node<T> {
    private final T value;
    private Optional<Node<T>> next = Optional.empty();
    private Optional<Node<T>> prev = Optional.empty();

    Node(T value) {
        this.value = value;
    }

    T getValue()   { return value; }
    Optional<Node<T>> getNext() { return next; }
    Optional<Node<T>> getPrev() { return prev; }

    void setNext(Optional<Node<T>> node) { next = node; }
    void setPrev(Optional<Node<T>> node) { prev = node; }
}
