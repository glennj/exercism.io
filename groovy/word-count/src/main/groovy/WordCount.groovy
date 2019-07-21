class WordCount {

    final sentence

    WordCount(s) {
        this.sentence = s
    }

    def wordCount() {
        def words = this.sentence
            .toLowerCase()
            .findAll("\\p{Alnum}+('\\p{Alnum}+)?")
            //.groupBy { it }
            //.collectEntries { key, val -> [key, val.size()] }
            .countBy { it }
    }
}
