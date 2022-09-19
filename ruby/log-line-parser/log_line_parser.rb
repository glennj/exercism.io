class LogLineParser
  def initialize(line)
    @line = line
  end

  def message
    @line[/:\K.*/].strip
  end

  def log_level
    @line[/(?<=\[).+?(?=\])/].downcase
  end

  def reformat
    "#{message} (#{log_level})"
  end
end
