class Forth {
    private val stack = Stack<Int>()
    private val macros = mutableMapOf<String, List<String>>()

    fun evaluate(vararg line: String): List<Int> {
        line.forEach {
            evaluateInstruction(it.toLowerCase())
        }
        return stack.toList()
    }

    private fun evaluateInstruction(line: String) {
        val tokens = line.split(' ').toMutableList()
        while (tokens.isNotEmpty()) {
            when(val item = tokens.removeAt(0)) {
                ":" -> {
                    defineMacro(tokens)
                    tokens.removeAll {true}
                }
                in macros -> tokens.addAll(0, macros[item] as Collection<String>)
                in listOf("+", "-", "*", "/") -> binaryArithmeticOperation(item)
                in listOf("dup", "drop", "over", "swap") -> stackOperation(item)
                else -> {
                    require(item.all(Char::isDigit)) {"undefined operation"}
                    stack.push(item.toInt())
                }
            }
        }
    }

    private fun need(n: Int) {
        if (n > 0) require(stack.isNotEmpty()) {"empty stack"}
        if (n > 1) require(stack.size > 1) {"only one value on the stack"}
    }

    private fun binaryArithmeticOperation(operation: String) {
        need(2)
        val b = stack.pop()
        val a = stack.pop()
        stack.push(when(operation) {
            "+" -> a + b
            "-" -> a - b
            "*" -> a * b
            else -> {
                require(b != 0) {"divide by zero"}
                a / b
            }
        })
    }

    private fun stackOperation(operation: String) {
        when (operation) {
            "dup" -> {
                need(1)
                stack.push(stack.peek())
            }
            "drop" -> {
                need(1)
                stack.pop()
            }
            "over" -> {
                need(2)
                val b = stack.pop()
                val a = stack.peek()
                stack.push(b)
                stack.push(a)
            }
            "swap" -> {
                need(2)
                val b = stack.pop()
                val a = stack.pop()
                stack.push(b)
                stack.push(a)
            }
        }
    }

    private fun defineMacro(tokens: List<String>) {
        require(tokens.last() == ";") {"malformed macro definition"}
        val name = tokens.first()
        require(!name.all(Char::isDigit)) {"illegal operation"}
        val words = tokens.drop(1).dropLast(1)

        val definition = mutableListOf<String>()
        for (word in words) {
            if (word in macros)
                definition.addAll(macros[word]!!)
            else
                definition.add(word)
        }
        macros[name] = definition

    }
}

class Stack<T> {
    private val stack = mutableListOf<T>()
    fun push(elem: T) {
        stack.add(elem)
    }
    fun peek(): T = stack.last()
    fun pop(): T = stack.removeAt(stack.lastIndex)
    fun isEmpty(): Boolean = stack.isEmpty()
    fun isNotEmpty(): Boolean = !isEmpty()
    val size: Int get() = stack.size
    fun toList(): List<T> = stack.toList()
}
