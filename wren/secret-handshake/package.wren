import "wren-package" for WrenPackage, Dependency
import "os" for Process

class Package is WrenPackage {
  construct new() {}
  name { "exercism/secret-handshake" }
  dependencies {
    return [
      Dependency.new("wren-testie", "0.3.0", "https://github.com/joshgoebel/wren-testie.git")
    ]
  }
}

Package.new().default()
