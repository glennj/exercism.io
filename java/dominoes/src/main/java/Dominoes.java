import java.util.Collections;
import java.util.List;
import java.util.Vector;

public class Dominoes {

    List<Domino> formChain(List<Domino> dominoes) throws ChainNotFoundException {
        if (dominoes.size() == 0)
            return Collections.emptyList();

        for (int i = 0; i < dominoes.size(); i++) {
            Domino d = dominoes.get(i);
            List<Domino> rest = removeDomino(dominoes, i);
            Vector<Domino> initial = new Vector<>();
            initial.add(d);

            List<Domino> chain = makeChain(initial, rest);
            if (chain != null)
                return chain;

            if (d.getLeft() == d.getRight())
                continue;

            initial.set(0, reverseDomino(d));
            chain = makeChain(initial, rest);
            if (chain != null)
                return chain;
        }
        throw new ChainNotFoundException("No domino chain found.");
    }

    private List<Domino> makeChain(Vector<Domino> chain, List<Domino> remaining) {
        int head = chain.firstElement().getLeft();
        int tail = chain.lastElement().getRight();

        if (remaining.isEmpty())
            return head == tail ? chain : null;

        for (int i = 0; i < remaining.size(); i++) {
            Domino d = remaining.get(i);
            if (d.getLeft() != tail && d.getRight() != tail)
                continue;

            if (d.getRight() == tail)
                d = reverseDomino(d);

            List<Domino> rest = removeDomino(remaining, i);
            Vector<Domino> new_chain = new Vector<>(chain);
            new_chain.add(d);

            new_chain = (Vector<Domino>) makeChain(new_chain, rest);
            if (new_chain != null)
                return new_chain;
        }
        return null;
    }

    private Domino reverseDomino(Domino d) {
        return new Domino(d.getRight(), d.getLeft());
    }

    private List<Domino> removeDomino(List<Domino> pack, int index) {
        List<Domino> remaining = new Vector<>(pack);
        remaining.remove(index);
        return remaining;
    }
}
