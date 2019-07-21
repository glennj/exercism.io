class HighScores {

    def scores = []

    HighScores(scores) {
        this.scores = scores.clone()
    }

    def latest() { this.scores.last() }

    def personalBest() { this.scores.max() }

    def personalTopThree() {
        /*
        this.scores
            .sort(false)     // non-mutating
            .takeRight(3)
            .reverse()
        */
        this.scores
            .sort(false) {-it}    // non-mutating, reverse
            .take(3)
    }
}
