class CollatzConjecture {

    static int steps(int number) {
        if (number <= 0)
            throw new ArithmeticException()

        //stepsRecursive(number)
        stepsIterative(number)
    }

    private static int stepsRecursive(int number) {
        switch (number) {
            case 1: 
                return 0
            case {it % 2 == 0}:
                return 1 + steps( number.intdiv(2) )
            default: 
                return 1 + steps( 3 * number + 1 )
        }
    }

    private static int stepsIterative(int number) {
        def steps = 0
        while (number > 1) {
            steps++
            number = number % 2 == 0 
                        ? number.intdiv(2)
                        : 3 * number + 1
        }
        steps
    }
}
