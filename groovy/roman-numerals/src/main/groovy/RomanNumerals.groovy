class RomanNumerals {
    RomanNumerals() {
        Integer.metaClass.getRoman = { ->
            def n = delegate
            def rom = new StringBuilder()
            [   1000: 'M', 900: 'CM', 500: 'D', 400: 'CD',
                 100: 'C',  90: 'XC',  50: 'L',  40: 'XL',
                  10: 'X',   9: 'IX',   5: 'V',   4: 'IV',
                   1: 'I'
            ].each {m, r -> while (n >= m) {rom << r; n -= m}}
            rom.toString()
        }
    }
}
