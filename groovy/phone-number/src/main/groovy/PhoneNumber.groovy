class PhoneNumber {
    final String areaCode
    final String exchange
    final String last4
    final String number

    static final regex = '^1?([2-9]\\d{2})([2-9]\\d{2})(\\d{4})$'

    PhoneNumber(String input) {
        def m = input.replaceAll('\\D', '') =~ regex
        if (! m)
            throw new Exception('Invalid phone number')

        areaCode = m.group(1)
        exchange = m.group(2)
        last4 = m.group(3)
    }

    static clean(input) {
        def p = new PhoneNumber(input)
        return p.areaCode + p.exchange + p.last4
    }

    String toString() {
        "($areaCode) $exchange-$last4"
    }
}
