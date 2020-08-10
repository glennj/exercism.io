public class BinaryTree {
    private Zipper zipper;

    BinaryTree(int value) {
        this(new Zipper(value));
    }

    BinaryTree(Zipper zipper) {
        this.zipper = zipper;
        zipper.setTree(this);
    }

    Zipper getRoot() { return zipper; }

    String printTree() {
        StringBuilder sb = new StringBuilder();
        sb.append("value: ").append(zipper.value);

        sb.append(", left: ");
        if (zipper.left == null)
            sb.append("null");
        else
            sb.append("{ ").append(zipper.left.tree.printTree()).append(" }");

        sb.append(", right: ");
        if (zipper.right == null)
            sb.append("null");
        else
            sb.append("{ ").append(zipper.right.tree.printTree()).append(" }");

        return sb.toString();
    }
}
