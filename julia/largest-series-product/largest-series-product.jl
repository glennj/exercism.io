function largest_product(str, span)
	if span ∉ 0:length(str)
		throw(ArgumentError("invalid span"))
	end

	# parse will throw ArgumentError for non-digits
	digits  = [parse(Int, c) for c in str]

	# this is a generator, not an array
	series  = (view(digits, i:i+span-1) for i in 1:length(digits) - span + 1)

	maximum(prod, series)
end
