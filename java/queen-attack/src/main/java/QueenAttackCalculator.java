public class QueenAttackCalculator {
    private final Queen white;
    private final Queen black;

    QueenAttackCalculator(Queen white, Queen black) {
        validate(white, black);
        this.white = (Queen) white.setColour(ChessPiece.Colour.WHITE);
        this.black = (Queen) black.setColour(ChessPiece.Colour.BLACK);
    }

    private void validate(Queen white, Queen black) throws IllegalArgumentException {
        if (white == null || black == null)
            throw new IllegalArgumentException("You must supply valid positions for both Queens.");
        if (white.getPosition().equals(black.getPosition()))
            throw new IllegalArgumentException("Queens cannot occupy the same position.");
    }

    public boolean canQueensAttackOneAnother() {
        int dx = Math.abs(white.getPosition().getRank() - black.getPosition().getRank());
        int dy = Math.abs(white.getPosition().getFile() - black.getPosition().getFile());
        return dx == 0 || dy == 0 || dx == dy;
    }
}
