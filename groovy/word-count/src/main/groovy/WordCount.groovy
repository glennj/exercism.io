class WordCount {

    final sentence

    WordCount(s) {
        this.sentence = s
    }

    def countWords() {
        def words = this.sentence
            .toLowerCase()
            .findAll("\\p{Alnum}+('\\p{Alnum}+)?")
            .countBy { it }
    }
}
