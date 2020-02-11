object Flattener {
    /*
     * first take, simple loop, not using flatMap, handling null explicitly
     *
    fun flatten(source: Collection<Any?>): List<Any> {
        val result: MutableList<Any> = mutableListOf<Any>()
        for (elem in source) {
            if (elem is Collection<*>) {
                result.addAll(flatten(elem))
            } else {
                elem?.let { result.add(elem) }
            }
        }
        return result
    }
     *
     */

    fun flatten(source: Collection<Any?>): List<Any> = source
            .flatMap {
                when (it) {
                    is Collection<*> -> flatten(it)
                    else -> listOf(it)
                }
            }
            .filterNotNull()

}
