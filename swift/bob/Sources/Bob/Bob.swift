import Foundation

extension String {   
    func trim() -> String {
        return self.trimmingCharacters(in: .whitespacesAndNewlines)
    }
}

func hey(_ input: String) -> String {
    let trimmed = input.trim()

    // silence
    if trimmed.isEmpty { return "Fine. Be that way!" }

    // shouting: input contains letters but no lowercase letters
    let shouting: Bool = 
        // trimmed.rangeOfCharacter(from: .letters) != nil &&
        // trimmed.rangeOfCharacter(from: .lowercaseLetters) == nil
        trimmed == trimmed.uppercased() &&
        trimmed != trimmed.lowercased()
    if shouting { return "Whoa, chill out!" }

    // ask a question
    if trimmed.hasSuffix("?") { return "Sure." } 

    // anything else
    return "Whatever."
}