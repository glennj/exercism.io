class OcrNumbers
  def self.convert(input)
    new(input).output
  end

  attr_reader :output

  def initialize(input)
    lines = input.lines.map(&:chomp)
    raise ArgumentError, 'lines % 4' if lines.length.modulo(4).nonzero?
    raise ArgumentError, 'line % 3' if lines.any? { |l| (l.length % 3).nonzero? }

    @output = lines
                .each_slice(4)
                .map { |line_group| convert_line(line_group) }
                .join(',')
  end

  private

  DIGIT_STRINGS = [
    ' _ | ||_|   ',
    '     |  |   ',
    ' _  _||_    ',
    ' _  _| _|   ',
    '   |_|  |   ',
    ' _ |_  _|   ',
    ' _ |_ |_|   ',
    ' _   |  |   ',
    ' _ |_||_|   ',
    ' _ |_| _|   '
  ].freeze

  def convert_line(lines)
    (lines.first.length / 3)
      .times
      .each_with_object('') do |i, digits|
        digit_string = lines.map { |line| line.slice(3 * i, 3) }.join('')
        digits << digit_lookup(digit_string)
      end
  end

  def digit_lookup(digit_string)
    idx = DIGIT_STRINGS.index digit_string
    idx.nil? ? '?' : idx.to_s
  end
end
