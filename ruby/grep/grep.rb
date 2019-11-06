module Grep
  module_function

  def grep(pattern, flags, files)
    emit = emitter(flags, files)
    re = regexp(pattern, flags)

    file_only = flags.include? '-l'
    invert = flags.include? '-v'

    files
      .reduce([]) do |result, file|
        result.concat process_file(file, emit, re, file_only, invert)
      end
      .join "\n"
  end

  def self.process_file(file, emit, regex, file_only, invert)
    matches = []
    nr = 0
    File.foreach(file) do |line|
      line.chomp!
      nr += 1
      matched = line.match? regex
      if (!invert && matched) || (invert && !matched)
        matches << emit.call(file, nr, line)
        break if file_only
      end
    end
    matches
  end

  private_class_method :process_file

  def self.regexp(pattern, flags)
    opts = 0
    opts |= Regexp::IGNORECASE if flags.include? '-i'
    pattern = "^#{pattern}$" if flags.include? '-x'
    Regexp.new(pattern, opts)
  end

  private_class_method :regexp

  def self.emitter(flags, files)
    if flags.include? '-l'
      proc { |file, _, _| file }
    elsif flags.include? '-n'
      if files.length > 1
        proc { |file, line, string| [file, line, string].join ':' }
      else
        proc { |_, line, string| [line, string].join ':' }
      end
    elsif files.length > 1
      proc { |file, _, string| [file, string].join ':' }
    else
      proc { |_, _, string| string }
    end
  end

  private_class_method :emitter
end
