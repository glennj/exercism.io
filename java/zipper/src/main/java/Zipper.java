import java.util.ArrayList;
import java.util.List;

public class Zipper {
    private BinaryTree tree;
    private List<Zipper> path;
    public

    Zipper(int value) {
        this.tree = new BinaryTree(value, this);
        path = new ArrayList<>();
    }

    void setTree(BinaryTree tree) {
        this.tree = tree;
    }

    BinaryTree getTree() { return this.tree; }

    int getValue() { return this.tree.getValue(); }
    BinaryTree getLeft() { return this.tree.getLeft(); }
    BinaryTree getRight() { return this.tree.getRight(); }
}
