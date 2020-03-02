class Reactor<T : Any> {

    abstract inner class Cell() {
        abstract val value: T
        private val listeners = mutableListOf<ComputeCell>()

        internal fun addListener(cell: ComputeCell) {
            listeners.add(cell)
        }

        internal fun recomputeListeners() {
            listeners.forEach { it.compute() }
        }

        internal fun fireListenerCallbacks() {
            listeners.forEach { it.fireCallbacks() }
        }
    }

    /* ************************************************** */
    inner class InputCell(data: T) : Cell() {
        override var value = data
            set(newData) {
                field = newData
                recomputeListeners()
                fireListenerCallbacks()
            }
    }

    /* ************************************************** */
    inner class ComputeCell(private vararg var cells: Cell, private val transform: (List<T>) -> T) : Cell() {

        override lateinit var value: T    // type T cannot be nullable for lateinit
            private set

        init {
            compute()
            cells.forEach { it.addListener(this) }
        }

        private var previousValue = value
        private val callbacks = mutableListOf<Callback>()

        internal fun compute() {
            value = transform(cells.map { it.value })
            recomputeListeners()
        }

        fun addCallback(func: (T) -> Unit): Subscription {
            val callback = Callback(this, func)
            callbacks.add(callback)
            return callback
        }

        internal fun removeCallback(callback: Callback) {
            callbacks.remove(callback)
        }

        internal fun fireCallbacks() {
            if (value != previousValue) {
                previousValue = value
                callbacks.forEach { it.fire() }
                fireListenerCallbacks()
            }
        }
    }

    /* ************************************************** */
    interface Subscription {
        fun cancel()
    }

    inner class Callback(private val cell: ComputeCell, val callback: (T) -> Unit): Subscription {
        internal fun fire() {
            callback(cell.value)
        }

        override fun cancel() {
            cell.removeCallback(this)
        }
    }
}
