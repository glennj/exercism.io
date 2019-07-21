class AllYourBase {

    private decimal = 0

    AllYourBase(inputBase, digits) {
        if (inputBase < 2)
            throw new ArithmeticException("Invalid base")

        digits.each { d -> 
            if (! (0..<inputBase).contains(d))
                throw new ArithmeticException("Invalid digit")

            decimal = decimal * inputBase + d
        }
    }

    def rebase(outputBase) {
        if (outputBase < 2)
            throw new ArithmeticException("Invalid base")
        if (decimal == 0)
            return [0]

        def digits = []
        def n = decimal
        while (n > 0) {
            digits.push(n % outputBase)
            n = n.intdiv(outputBase)
        }
        digits
    }
}
