class Bob {
    hey( input: string ): string {
        if (this.isYelling(input))  { return 'Whoa, chill out!' }
        if (this.isQuestion(input)) { return 'Sure.' }
        if (this.isSilence(input))  { return 'Fine. Be that way!' }
        return 'Whatever.'
    }

    private isQuestion( input: string ): boolean {
        return /[?]\s*$/.test(input)
    }

    private isSilence( input: string ): boolean {
        return /^\s*$/.test(input)
    }

    private isYelling( input: string ): boolean {
        return /[A-Z]/.test(input)
            && input === input.toUpperCase()
    }
}

export default Bob
