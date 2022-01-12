enum class Orientation {

    NORTH, EAST, SOUTH, WEST;

    fun left(): Orientation =
        when (this) {
            WEST  -> SOUTH
            SOUTH -> EAST
            EAST  -> NORTH
            NORTH -> WEST
        }

    fun right(): Orientation =
        when (this) {
            WEST  -> NORTH
            SOUTH -> WEST
            EAST  -> SOUTH
            NORTH -> EAST
        }
}
