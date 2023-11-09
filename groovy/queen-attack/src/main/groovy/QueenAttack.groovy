class QueenAttack {
    static canAttack(Queen q1, Queen q2) {
        def rowDiff = (q1.row - q2.row).abs()
        def columnDiff = (q1.column - q2.column).abs()
        return rowDiff == 0 || columnDiff == 0 || rowDiff == columnDiff
    }
}
