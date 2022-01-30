import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

class BinarySearchTree<T extends Comparable<T>> {
    private Node<T> root;

    Node<T> getRoot() { return root; }

    void insert(T value) {
        if (root == null)
            root = new Node<>(value);
        else
            root.insert(value);
    }

    List<T> getAsSortedList() {
        return root.getAsSortedList();
    }

    List<T> getAsLevelOrderList() {
        List<T> values = new ArrayList<>();
        Queue<Node<T>> queue = new LinkedList<>();
        queue.add(root);

        while (!queue.isEmpty()) {
            Node<T> node = queue.remove();
            values.add(node.getData());
            
            if (node.getLeft() != null)
                queue.add(node.getLeft());
            if (node.getRight() != null)
                queue.add(node.getRight());
        }

        return values;
    }

    /* ********************************************************************** */
    static class Node<T extends Comparable<T>> {
        private final T data;
        private Node<T> left;
        private Node<T> right;

        Node(T data) { this.data = data; }

        Node<T> getLeft()  { return left; }
        Node<T> getRight() { return right; }
        T       getData()  { return data; }

        void insert(T value) {
            int cmp = value.compareTo(data);
            if (cmp <= 0)
                if (left == null)
                    left = new Node<>(value);
                else
                    left.insert(value);
            else if (cmp > 0)
                if (right == null)
                    right = new Node<>(value);
                else
                    right.insert(value);
        }

        List<T> getAsSortedList() {
            List<T> values = new ArrayList<>();
            if (left != null)
                values.addAll(left.getAsSortedList());
            values.add(data);
            if (right != null)
                values.addAll(right.getAsSortedList());
            return values;
        }
    }
}
