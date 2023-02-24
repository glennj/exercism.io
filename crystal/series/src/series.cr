module Series
  def self.slices(series : String, slice_length : Number) : Array(String)
    raise ArgumentError.new unless (1 .. series.size).includes? slice_length
    series.chars
          .each_cons(slice_length)
          .map(&.join)
          .to_a
  end
end
