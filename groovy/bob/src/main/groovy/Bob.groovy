class Bob {

    static response(input) {
        input = input.trim()

        def isSilent   = input.isEmpty()
        def isQuestion = input.endsWith("?")
        // contains a letter, but no lowercase letters
        def isShouting = input.find("\\p{Alpha}") && 
                         ! input.find("\\p{Lower}")

        if (isSilent) 
            return 'Fine. Be that way!'
        if (isQuestion && isShouting) 
            return "Calm down, I know what I'm doing!"
        if (isQuestion) 
            return 'Sure.'
        if (isShouting)
            return 'Whoa, chill out!'
        return 'Whatever.'
    }
}
