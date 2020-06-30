class Microwave
  def initialize(input)
    @time = input
  end

  def timer
    # The input here a some number of digits.
    # The last 2 digits represents the number of seconds.
    # Any preceding digits are the number of minutes.
    #
    # For example, the input "990" will represent
    # 9 minutes and 90 seconds, or 10:30 for the display.

    minutes, seconds = @time.divmod 100
    if seconds >= 60
      seconds %= 60
      minutes += 1
    end
    sprintf "%02d:%02d", minutes, seconds
  end
end
