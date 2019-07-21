class PrimeFactors {

    static factors(value) {
        def factors = []
        def f = 2
        while (f * f <= value) {
            switch (value % f) {
                case 0:
                    factors << f
                    value = value.intdiv(f)
                    break
                default:
                    f += f == 2 ? 1 : 2
            }
        }
        if (value > 1)
            factors << value
        factors
    }
}
