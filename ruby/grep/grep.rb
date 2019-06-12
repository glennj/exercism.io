module Grep

  module_function

  def grep(pattern, flags, files)
    emit = emitter(flags, files)
    re = regexp(pattern, flags)

    file_only = flags.include? "-l"
    invert = flags.include? "-v"

    result = []
    files.each do |file|
      nr = 0
      File.foreach(file) do |line|
        line.chomp!
        nr += 1
        matched = line.match? re
        if (not invert and matched) or (invert and not matched) 
          result << emit.call(file, nr, line) 
          break if file_only
        end
      end
    end
    result.join "\n"
  end


  private

  def self.regexp(pattern, flags)
    opts = 0
    opts |= Regexp::IGNORECASE if flags.include? "-i"
    pattern = "^#{pattern}$" if flags.include? "-x"
    Regexp.new(pattern, opts)
  end

  def self.emitter(flags, files)
    if flags.include? "-l"
      Proc.new {|file, line, string| file}
    elsif flags.include? "-n"
      if files.length > 1
        Proc.new {|file, line, string| [file, line, string].join ":"} 
      else
        Proc.new {|file, line, string| [line, string].join ":"} 
      end
    else
      if files.length > 1
        Proc.new {|file, line, string| [file, string].join ":"} 
      else
        Proc.new {|file, line, string| string}
      end
    end
  end
end
