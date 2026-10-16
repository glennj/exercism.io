# A slightly different approach to making the main func recursive:
# define an inner recursive lambda. This comes with quirks
func flatten(iterable):
	var result = []

	var flattener: Callable
	# need to pass the lambda as a parameter!
	# if I used `flattener.call` inside, then the compiler
	# takes its current value, which is null
	flattener = func(recur, a):
		for elem in a:
			if typeof(elem) == TYPE_ARRAY:
				recur.call(recur, elem)
			elif elem != null:
				result.append(elem)

	flattener.call(flattener, iterable)
	return result
