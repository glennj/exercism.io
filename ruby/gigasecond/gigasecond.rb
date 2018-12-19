# a module that adds 1,000,000,000 to things
module Gigasecond
  module_function

  def from(time)
    time + 1_000_000_000
  end
end
