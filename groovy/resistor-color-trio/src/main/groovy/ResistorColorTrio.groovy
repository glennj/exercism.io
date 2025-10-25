// The max resistor value, 99 * 10^9 is bigger than a 32-bit integer.
// The resistor value needs to be expressed as a BigInteger.

class ResistorColorTrio {
    static colors = [
        'black', 'brown', 'red', 'orange', 'yellow', 
        'green', 'blue', 'violet', 'grey', 'white'
    ]

    static prefix = ['', 'kilo', 'mega', 'giga']

    static String label(List<String> colorsInput) {
        def value = resistorValue(colorsInput)

        def magnitude = 0
        while (value > 2 && value % 1000 == 0) {
            magnitude++
            value /= 1000
        }
        "$value ${prefix[magnitude]}ohms"
    }

    static private resistorValue(List<String> input) {
        def (a, b, c) = input.take(3)
                             .collect { colors.indexOf(it) }
                             .collect { BigInteger.valueOf(it) }

        (10 * a + b) * BigInteger.TEN.pow(c)
    }
}
