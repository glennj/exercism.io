class ChessPiece {

    enum Colour { WHITE, BLACK }
    enum Type { King, Queen, Bishop, Knight, Rook, Pawn }

    private ChessBoard.Position position;
    private Type type;
    private Colour colour;

    ChessPiece(int row, int col) {
        position = new ChessBoard.Position(row, col);
    }

    ChessPiece setType(Type type) { this.type = type; return this; }
    ChessPiece setColour(Colour colour) { this.colour = colour; return this; }
    ChessPiece setPosition(ChessBoard.Position position) {
        this.position = position;
        return this;
    }

    Type getType() { return type; }
    ChessBoard.Position getPosition() { return position; }
    Colour getColour() { return colour; }

    // TODO logic about legal moves, etc.
}
