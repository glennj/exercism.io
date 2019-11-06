module PhoneNumber
  module_function

  def clean(input)
    return unless input =~ /\d{3}.*\d{3}.*\d{4}/

    num = input.gsub(/\D/, '').sub(/^1/, '')
    # 10 digits, with 0,1 not allowed to start area code or exchange
    return num if num =~ /^(?:[^01]..)(?:[^01]..).{4}$/
  end
end
