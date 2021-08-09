import "wren-package" for WrenPackage, Dependency
import "os" for Process

class Package is WrenPackage {
  construct new() {}
  name { "exercism/rna-transcription" }
  dependencies {
    return [
      Dependency.new("wren-testie", "0.1.2", "https://github.com/joshgoebel/wren-testie.git")
    ]
  }
}

Package.new().default()
