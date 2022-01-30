class Luhn {
    static luhnDigits = [
        [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
        [0, 2, 4, 6, 8, 1, 3, 5, 7, 9]
    ]

    static boolean valid(String value)  {
        value = value.replaceAll(' ', '')

        if (value.size() < 2 || value.find('\\D'))
            return false

        value.reverse()
             .toList()
             .withIndex()
             .collect { luhnDigit(*it) }
             .sum() % 10 == 0
    }

    private static luhnDigit(String digit, int idx) {
        luhnDigits[idx % 2][digit as int]
    }
}
