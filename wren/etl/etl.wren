import "./byte" for Byte

class Etl {
  static transform(legacy) {
    return legacy.reduce({}) { |transformed, entry|
      entry.value.each { |letter|
        transformed[Byte.downcase(letter)] = entry.key
      }
      return transformed
    }
  }
}

