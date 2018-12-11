class Isogram {
    static isIsogram( input: string ): boolean {
        const word = input.toLowerCase().replace(/[^a-z]/g, '')
        const letters = new Set([...word])
        return word.length === letters.size
    }
}

export default Isogram
