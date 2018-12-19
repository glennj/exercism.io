export default class Triangle {

    sides: number[]

    constructor(...sides: number[]) {
        this.sides = sides
    }

    kind() {
        const [a, b, c] = this.sides.sort((a, b) => a - b)

        if (a <= 0 || (a + b) <= c) { throw new Error() }
        
        if (a === b && b === c)     { return 'equilateral' }
        if (a === b || b === c)     { return 'isosceles' }
        return 'scalene'
    }
}