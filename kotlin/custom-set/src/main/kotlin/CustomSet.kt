class CustomSet(vararg elements: Int) {

    // kinda feels like cheating to use a backing Set
    private val set = elements.toMutableSet()

    val size: Int
        get() = set.size

    fun isEmpty(): Boolean = size == 0

    fun isSubset(other: CustomSet): Boolean = set.all { other.contains(it) }

    fun isDisjoint(other: CustomSet): Boolean = set.none { other.contains(it) }

    fun contains(element: Int): Boolean = set.contains(element)

    fun add(element: Int) {
        set.add(element)
    }

    fun intersection(other: CustomSet): CustomSet = CustomSet(
        *set.filter { other.contains(it) }.toIntArray()
    )

    operator fun minus(other: CustomSet): CustomSet = CustomSet(
        *set.filter { !other.contains(it) }.toIntArray()
    )

    operator fun plus(other: CustomSet): CustomSet = CustomSet(
        *(set + other.minus(this).set).toIntArray()
    )

    override fun equals(other: Any?): Boolean = when(other) {
        is CustomSet -> this.size == other.size && this.isSubset(other)
        else -> false
    }

    override fun hashCode(): Int = set.hashCode()
}
