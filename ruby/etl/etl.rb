module ETL
  module_function

  def transform(data)
    data.to_a
        .each_with_object({}) do |(key, values), result|
          values.each { |value| result[value.downcase] = key }
        end
  end
end
