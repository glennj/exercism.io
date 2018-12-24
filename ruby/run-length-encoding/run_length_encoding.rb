module RunLengthEncoding
  module_function

  def encode(input)
    input.scan(/((.)\2*)/).reduce('') do |encoded, (run, char)|
      encoded + (run.length > 1 ? run.length.to_s : '') + char
    end
  end

  def decode(rle)
    rle.scan(/(\d*)(\D)/).reduce('') do |decoded, (len, char)|
      decoded + char * (len.empty? ? '1' : len).to_i
    end
  end
end
