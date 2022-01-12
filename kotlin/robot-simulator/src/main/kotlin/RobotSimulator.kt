import Orientation.*

class Robot() {

    var gridPosition: GridPosition
        private set

    var orientation: Orientation
        private set

    init {
        gridPosition = GridPosition(0, 0)
        orientation = NORTH
    }

    constructor(gridPosition: GridPosition, orientation: Orientation) : this() {
        this.gridPosition = gridPosition
        this.orientation = orientation
    }

    fun simulate(instructions: String) {
        for (i in instructions) {
            when (i) {
                'L' -> turnLeft()
                'R' -> turnRight()
                'A' -> advance()
                else -> throw IllegalArgumentException()
            }
        }
    }

    private fun turnLeft() {
        orientation = orientation.left()
    }

    private fun turnRight() {
        orientation = orientation.right()
    }

    private fun advance() {
        gridPosition = gridPosition.advance(orientation)
    }
}
