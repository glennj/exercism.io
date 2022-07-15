import "io" for File
//import "essentials" for Strings
import "./byte" for Byte

class Grep {
  static grep(pattern, files) {
    return grep(pattern, files, [])
  }

  static grep(pattern, files, flags) {
    var grepper = new(pattern, files, flags)
    return grepper.run
  }

  construct new(pattern, files, flags) {
    _flags = {"-i": false, "-l": false, "-n": false, "-v": false, "-x": false}
    for (f in flags) { _flags[f] = true }
    
    _files = files
    _pattern = pattern

    //if ( _flags["-i"]) { _pattern = Strings.downcase(pattern) }
    // ref https://github.com/joshgoebel/wren-essentials/blob/0d116a1658fdcd3a33496e5e0f69fbe9f01e5b38/src/modules/strings.wren#L29
    //if (!_flags["-x"]) { _pattern = "*" + _pattern + "*" }

    if ( _flags["-i"]) { _pattern = Byte.downcase(pattern) }
  }

  run {
    return _files.reduce([]) {|results, file|
      var file_matches = process_file(file)
      results.addAll(file_matches)
      return results
    }
  }

  process_file(file) {
    var lines = File.read(file).split("\n")
    var result = []
    for (i in (0...lines.count)) {
      var line = lines[i]
      if (matches(line)) {
        if (_flags["-l"]) {
          result = [file]
          break
        }
        result.add(output(line, i + 1, file))
      }
    }
    return result
  }

  matches(line) { 
    //var cmp_line = _flags["-i"] ? Strings.downcase(line) : line
    //var is_match = Strings.globMatch(cmp_line, _pattern)

    var cmp_line = _flags["-i"] ? Byte.downcase(line) : line
    var is_match = _flags["-x"] ? (cmp_line == _pattern) : cmp_line.contains(_pattern)

    return (is_match && !_flags["-v"]) || (!is_match && _flags["-v"])  // xor
  }
  
  output(line, line_num, file) {
    var prefix = ""
    if (_files.count > 1) { prefix = "%(prefix)%(file):" }
    if (_flags["-n"]) { prefix = "%(prefix)%(line_num):" }
    return prefix + line
  }
}
