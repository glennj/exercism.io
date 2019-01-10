import java.util.Optional;

public class DoublyLinkedList<T> {

    private Optional<Node<T>> head = Optional.empty();
    private Optional<Node<T>> tail = Optional.empty();

    // adding elements to list

    public void push(T element) {
        Optional<Node<T>> node = Optional.of(new Node<>(element));
        if (!tail.isPresent()) {
            head = node;
        }
        else {
            node.get().setPrev(tail);
            tail.get().setNext(node);
        }
        tail = node;
    }

    public void unshift(T element) {
        Optional<Node<T>> node = Optional.of(new Node<>(element));
        if (!head.isPresent()) {
            tail = node;
        }
        else {
            node.get().setNext(head);
            head.get().setPrev(node);
        }
        head = node;
    }

    // removing elements from list

    public T pop() {
        if (!tail.isPresent()) return null;

        Node<T> node = tail.get();
        if (!node.getPrev().isPresent()) {
            tail = Optional.empty();
            head = Optional.empty();
        }
        else {
            node.getPrev().get().setNext(Optional.empty());
            tail = node.getPrev();
        }
        return node.getValue();
    }

    public T shift() {
        if (!head.isPresent()) return null;

        Node<T> node = head.get();
        if (!node.getNext().isPresent()) {
            tail = Optional.empty();
            head = Optional.empty();
        }
        else {
            node.getNext().get().setPrev(Optional.empty());
            head = node.getNext();
        }
        return node.getValue();
    }
}
