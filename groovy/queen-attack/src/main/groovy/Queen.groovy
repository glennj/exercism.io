class Queen {
    final int row
    final int column

    private static final POSITION_RANGE = new NumberRange(0, 7)

    Queen(int row, int column) {
        check(row)
        check(column)
        this.row = row
        this.column = column
    }

    def check(int value) {
        if (!POSITION_RANGE.containsWithinBounds(value))
            throw new Exception("not on board")
    }
}
