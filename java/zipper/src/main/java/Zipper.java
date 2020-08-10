public class Zipper {
    BinaryTree tree;
    int value;
    Zipper left;
    Zipper right;
    Zipper up;

    Zipper(int value) {
        setValue(value);
        setTree(new BinaryTree(this));
    }

    void setValue(int value)      { this.value = value; }
    void setTree(BinaryTree tree) { this.tree = tree; }

    BinaryTree toTree()  {
        Zipper root = this;
        while (root.up != null)
            root = root.up;
        return root.tree;
    }

    int getValue()    { return value; }
    Zipper getLeft()  { return left; }
    Zipper getRight() { return right; }

    void setLeft(Zipper zipper)  {
        if (zipper != null)
            zipper.up = this;
        left = zipper;
    }

    void setRight(Zipper zipper) {
        if (zipper != null)
            zipper.up = this;
        right = zipper;
    }
}
