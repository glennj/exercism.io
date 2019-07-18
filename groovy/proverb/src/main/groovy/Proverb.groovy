class Proverb {

    static recite(strings) {
        if (strings.isEmpty()) return ''

        def lines = strings
            .collate(2, 1, false)
            .collect { "For want of a ${it.first()} the ${it.last()} was lost." }

        lines << "And all for the want of a ${strings.first()}."
        lines.join("\n")
    }
}
