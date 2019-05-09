// seconds per Earth year
let Year: Double = 31557600.0

// relative orbit compared to Earth
let RelativeOrbit: [String: Double] = [
    "Mercury":   0.2408467,
    "Venus":     0.61519726,
    "Earth":     1.0,
    "Mars":      1.8808158,
    "Jupiter":  11.862615,
    "Saturn":   29.447498,
    "Uranus":   84.016846,
    "Neptune": 164.79132,
]

struct SpaceAge {
    let seconds: Double
    init(_ age: Double) {
        seconds = age
    }

    private func age(on planet: String) -> Double {
        if let orbit = RelativeOrbit[planet] {
            return self.seconds / (Year * orbit)
        } else {
            // no such planet, return NaN
            return Double.nan
        }
    }

    var onEarth:   Double { return self.age(on: "Earth") }
    var onMercury: Double { return self.age(on: "Mercury") }
    var onVenus:   Double { return self.age(on: "Venus") }
    var onMars:    Double { return self.age(on: "Mars") }
    var onJupiter: Double { return self.age(on: "Jupiter") }
    var onSaturn:  Double { return self.age(on: "Saturn") }
    var onUranus:  Double { return self.age(on: "Uranus") }
    var onNeptune: Double { return self.age(on: "Neptune") }
}
