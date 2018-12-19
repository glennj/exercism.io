module FlattenArray
  module_function

  def flatten(something)
    # could be just
    # something.flatten.compact

    something.each_with_object([]) do |elem, flattened|
      next if elem.nil?
      flattened.concat(elem.is_a?(Array) ? flatten(elem) : Array(elem))
    end
  end
end
