import Foundation


struct ScaleGenerator {
    private static let sharpNotes = ["A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"]
    private static let flatNotes =  ["A", "Bb", "B", "C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab"]
    private static let flats = ["F", "Bb", "Eb", "Ab", "Db", "Gb", "d", "g", "c", "f", "bb", "eb"]

    private enum Interval: Int {
        case m = 1, M, A
        init(_ char: Character) {
            switch char {
            case "m": self = .m
            case "M": self = .M
            default:  self = .A
            }
        }
        var interval: Int { get {return rawValue} }
    }

    let name: String
    private var scale: [String] = []

    // TODO: input validation, but not required by test suite
    init( tonic:     String,
          scaleName: String, 
          pattern:   String = String(repeating: "m", count: 12)
    ) {
        self.name  = "\(tonic.capitalized) \(scaleName)"
        self.scale = self.generateScale(tonic, pattern)
    }

    func pitches() -> [String] {
        return self.scale
    }

    private func generateScale(_ tonic: String, _ pattern: String) -> [String] {
        var notes = ScaleGenerator.flats.contains(tonic)
            ? ScaleGenerator.flatNotes
            : ScaleGenerator.sharpNotes

        // rotate the notes
        var idx = notes.firstIndex(of: tonic.capitalized)!
        notes = Array(
            notes[idx ..< notes.count] +
            notes[0 ..< idx]
        )

        // now, extract based on the intervals in the pattern
        var pitches: [String] = []
        idx = 0
        for i in pattern {
            pitches.append(notes[idx])
            idx += Interval(i).interval
        }
        return pitches
    }
}