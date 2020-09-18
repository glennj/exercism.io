import java.util.HashSet;
import java.util.List;
import java.util.Set;

class Satellite {

    public Tree treeFromTraversals(List<Character> preOrder, List<Character> inOrder) {
        return new Tree(buildRootNode(preOrder, inOrder));
    }

    private Node buildRootNode(List<Character> preOrder, List<Character> inOrder) {
        validateInput(preOrder, inOrder);

        if (preOrder.isEmpty())
            return null;

        // first element of the preOrder is the tree's root
        Character root = preOrder.get(0);
        int idx = inOrder.indexOf(root);
        int len = preOrder.size();

        return new Node(
            root,
            buildRootNode(preOrder.subList(1, idx+1),   inOrder.subList(0, idx)),
            buildRootNode(preOrder.subList(idx+1, len), inOrder.subList(idx+1, len))
        );
    }

    private void validateInput(List<Character> preOrder, List<Character> inOrder) {
        if (preOrder.size() != inOrder.size())
            throw new IllegalArgumentException("traversals must have the same length");

        for (Character p : preOrder)
            if (!inOrder.contains(p))
                throw new IllegalArgumentException("traversals must have the same elements");

        Set<Character> chars;
        chars = new HashSet<>(preOrder);
        if (chars.size() != preOrder.size())
            throw new IllegalArgumentException("traversals must contain unique items");

        chars = new HashSet<>(inOrder);
        if (chars.size() != inOrder.size())
            throw new IllegalArgumentException("traversals must contain unique items");

    }
}
