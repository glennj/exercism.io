enum class Relationship {

    EQUAL, SUBLIST, SUPERLIST, UNEQUAL

}

fun <T> List<T>.relationshipTo(other: List<T>): Relationship {
    if (this == other)
        return Relationship.EQUAL

    if (this.size < other.size && this.isEmpty() || this in other.windowed(this.size))
        return Relationship.SUBLIST

    if (this.size > other.size && other.isEmpty() || other in this.windowed(other.size))
        return Relationship.SUPERLIST

    return Relationship.UNEQUAL
}
