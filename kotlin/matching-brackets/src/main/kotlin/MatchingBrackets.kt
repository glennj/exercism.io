class Stack<T> {
    private val stack = mutableListOf<T>()
    fun push(elem: T) {
        stack.add(elem)
    }
    fun peek(): T = stack.last()
    fun pop(): T = stack.removeAt(stack.lastIndex)
    fun isEmpty(): Boolean = stack.isEmpty()
}

object MatchingBrackets {
    private val bracketMap = mapOf('}' to '{', ']' to '[', ')' to '(')

    fun isValid(input: String): Boolean {
        val stack = Stack<Char>()

        for (c in input) {
            when (c) {
                in bracketMap.values -> stack.push(c)
                in bracketMap.keys -> {
                    if (stack.isEmpty() || stack.pop() != bracketMap[c])
                        return false
                }
            }
        }

        return stack.isEmpty()
    }
}
