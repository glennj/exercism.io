fun reverseIterative(input: String): String {
    val stringBuilder = StringBuilder(input.length)
    for (c in input) {
        stringBuilder.insert(0, c)
    }
    return stringBuilder.toString()
}


fun reverseFunctional(input: String): String = input
        .fold(StringBuilder(input.length)) { sb, c ->
            sb.insert(0, c)
        }.toString()


fun reverseStreamed(input: String): String = input
        .lastIndex
        .downTo(0)
        .map { input[it] }
        .joinToString("")


tailrec fun reverseRecursive(
        input: String,
        stringBuilder: StringBuilder = StringBuilder(input.length)
): String {

    if (input.isEmpty())
        return stringBuilder.toString()

    return reverseRecursive(
            input.substring(startIndex = 1),
            stringBuilder.insert(0, input.first())
    )
}


   fun reverse(input: String): String = reverseIterative(input)            // 13 ms
// fun reverse(input: String): String = reverseFunctional(input)           // 13 ms
// fun reverse(input: String): String = reverseRecursive(input)            // 20 ms
// fun reverse(input: String): String = reverseStreamed(input)             // 23 ms
