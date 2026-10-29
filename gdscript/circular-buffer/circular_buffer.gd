class CircularBuffer:
	var buffer: Array[int]
	var pointer: Dictionary[String, int]
	var capacity: int
	var count: int

	func _init(capacity):
		self.capacity = capacity
		buffer = []
		buffer.resize(capacity)
		clear()

	func clear():
		pointer = {read = 0, write = 0}
		count = 0

	func is_empty(): return count == 0

	func is_full(): return count == capacity

	func write(value):
		if is_full(): return false

		buffer[pointer.write] = value
		incr_pointer("write")
		count += 1
		return true

	func read():
		if is_empty(): return null

		var value = buffer[pointer.read]
		incr_pointer("read")
		count -= 1
		return value

	func overwrite(value):
		if is_full(): read()
		write(value)

	func incr_pointer(what):
		pointer[what] = (pointer[what] + 1) % capacity
