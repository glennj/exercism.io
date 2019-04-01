public class BinaryTree {
    private int value;
    private BinaryTree left;
    private BinaryTree right;
    private Zipper zipper;

    BinaryTree(int value, Zipper zipper) {
        this.value = value;
        setZipper(zipper);
        zipper.setTree(this);
    }

    BinaryTree(Zipper zipper) {
        setZipper(zipper);
        zipper.setTree(this);
    }

    void setZipper(Zipper zipper) {
        this.zipper = zipper;
    }

    Zipper getRoot() { return this.zipper; }

    int getValue() { return this.value; }
    BinaryTree getLeft() { return this.left; }
    BinaryTree getRight() { return this.right; }
}
