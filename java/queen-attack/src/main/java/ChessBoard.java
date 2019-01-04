class ChessBoard {
    static final int SIZE = 8;

    static class Position {
        private int rank;
        private int file;

        Position(int rank, int file) {
            validate(rank, "row");
            validate(file, "column");
            this.rank = rank;
            this.file = file;
        }

        int getRank() { return rank; }
        int getFile() { return file; }

        boolean equals(Position other) {
            return this.getRank() == other.getRank() &&
                   this.getFile() == other.getFile();
        }

        // TODO remove word "Queen" from exception messages.
        private void validate(int val, String what) throws IllegalArgumentException {
            if (val < 0)
                throw new IllegalArgumentException(
                        "Queen position must have positive " + what + "."
                );
            if (val >= SIZE)
                throw new IllegalArgumentException(
                        "Queen position must have " + what + " <= " + (SIZE - 1) + "."
                );
        }
    }

    // TODO rest of implementation
}
