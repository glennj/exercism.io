object Bob {
    fun hey(input: String): String {
        return with(input.trimEnd()) {
            val silence  = this.isEmpty()
            val asking   = this.endsWith('?')
            val shouting = this.contains(Regex("\\p{Upper}"))
                       && !this.contains(Regex("\\p{Lower}"))
            when {
                silence -> "Fine. Be that way!"
                shouting && asking -> "Calm down, I know what I'm doing!"
                shouting -> "Whoa, chill out!"
                asking -> "Sure."
                else -> "Whatever."
            }
        }
    }
}
