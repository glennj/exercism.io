class Set:
	var _data: Dictionary[int, bool]

	func _init(elements = []):
		_data = {}
		for e in elements:
			_data[e] = true

	func empty():           return _data.is_empty()
	func size():            return _data.size()
	func values():          return _data.keys()
	func contains(element): return _data.has(element)

	func subset(other):
		for val in _data:
			if not other.contains(val):
				return false
		return true

	func disjoint(other):
		for val in _data:
			if other.contains(val):
				return false
		return true

	func equal(other):
		return size() == other.size() and subset(other)

	func add(element):
		return new(values() + [element])

	func intersection(other):
		return new(values().filter(func(e): return other.contains(e)))

	func difference(other):
		return new(values().filter(func(e): return not other.contains(e)))

	func union(other):
		return new(values() + other.values())
