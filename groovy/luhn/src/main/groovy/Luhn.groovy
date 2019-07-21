class Luhn {

    static boolean valid(String value) {
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
        def d = digit as int
        if (idx % 2 == 1) {
            d *= 2
            if (d > 9) d -= 9
        }
        d
    }

}
