class CollatzConjecture {
    static steps(n: number, step: number = 0): number {
        if (n <= 0)  {
            throw new Error('Only positive numbers are allowed')
        }
        if (n === 1) {
            return step
        }
        if (n % 2 === 0) {
            n /= 2
        } else {
            n = 3 * n + 1
        }
        return this.steps(n, step + 1)
    }
}

export default CollatzConjecture