func slices(series: String, slice_length: int):
	if series.is_empty() or slice_length > series.length() or slice_length < 1:
		return null

	return range(series.length() - slice_length + 1) \
			.map(func (i): return series.substr(i, slice_length))
