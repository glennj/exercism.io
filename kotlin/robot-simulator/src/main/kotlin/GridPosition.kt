import Orientation.*

data class GridPosition(val x: Int, val y: Int) {

    fun advance(orientation: Orientation): GridPosition =
        when (orientation) {
            NORTH -> GridPosition(x, y+1)
            EAST  -> GridPosition(x+1, y)
            SOUTH -> GridPosition(x, y-1)
            WEST  -> GridPosition(x-1, y)
        }
}
