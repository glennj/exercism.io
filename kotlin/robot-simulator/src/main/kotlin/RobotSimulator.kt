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
        orientation = when (orientation) {
            WEST  -> SOUTH
            SOUTH -> EAST
            EAST  -> NORTH
            NORTH -> WEST
        }
    }

    private fun turnRight() {
        orientation = when (orientation) {
            WEST  -> NORTH
            SOUTH -> WEST
            EAST  -> SOUTH
            NORTH -> EAST
        }
    }

    private fun advance() {
        var (x, y) = gridPosition
        when (orientation) {
            NORTH -> y++
            EAST  -> x++
            SOUTH -> y--
            WEST  -> x--
        }
        gridPosition = GridPosition(x, y)
    }
}
