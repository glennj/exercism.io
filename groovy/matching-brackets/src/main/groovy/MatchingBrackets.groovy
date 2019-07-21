class MatchingBrackets {
    static private final brackets = [ ']':'[', '}':'{', ')':'(' ]

    static isPaired(value) {
        def stack = []

        for (ch in value) {
            if (brackets.any {k, v -> v == ch}) {
                stack.push(ch)
            }
            else if (brackets.any {k, v -> k == ch}) {
                if (stack.isEmpty()) return false
                if (stack.pop() != brackets[ch]) return false
            }
        }

        return stack.isEmpty()
    }
}
