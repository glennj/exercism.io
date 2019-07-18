class Anagram {
    def subject, key

    Anagram(subject) {
        this.subject = subject.toLowerCase()
        this.key = keyString(subject)
    }

    def keyString(str) {
        str.toLowerCase().toList().sort().join("")
    }

    def find(candidates) {
        candidates.findAll { 
            this.subject != it.toLowerCase() &&
            this.key == keyString(it) 
        }
    }
}
