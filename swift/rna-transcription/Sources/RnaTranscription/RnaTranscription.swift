enum TranscriptionError: Error {
    case invalidNucleotide(String)
}

struct Nucleotide {
    static let dna2rna: [Character: Character] = [
        "G": "C",
        "C": "G",
        "A": "U",
        "T": "A"
    ]
    let dna: String

    init(_ dna: String) {
        self.dna = dna
    }

    func complementOfDNA() throws -> String {
        return try self.dna.reduce(into: "") { rna, char in
            if let nucleotide = Nucleotide.dna2rna[char] {
                rna.append(nucleotide)
            } else {
                throw TranscriptionError.invalidNucleotide("""
                    \(char) is not a valid Nucleotide
                    """)
            }
        }
    }
}