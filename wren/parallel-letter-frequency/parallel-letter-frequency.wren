import "./frequency" for Frequency 

class ParallelLetterFrequency {
  construct new(inputs) {
    _texts = inputs
  }

  calculateFrequencies() {
    // create a fiber for each input text
    var fibers = _texts.map {|text|
      return Fiber.new {|str|
        Fiber.yield()   // so the first `call` doesn't wait for the result
        var freq = Frequency.ofText(str)
        Fiber.yield(freq)
      }
    }.toList

    // then send a piece of text to each fiber
    (0..._texts.count).each {|i|
      fibers[i].call(_texts[i])
    }

    // then collect the results
    var result = Frequency.new()
    fibers.each {|fiber|
      var freq = fiber.call()
      result.addAll(freq)
    }

    return result.toMap
  }
}
