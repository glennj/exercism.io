class ListNode:
	var value = null
	var next: ListNode

	func _init(value):
		self.value = value


# ------------------------------
class LinkedList:
	var head: ListNode

	func _init(values = []):
		for v in values: push(v)

	# the iterator protocol
	# https://docs.godotengine.org/en/stable/tutorials/scripting/gdscript/gdscript_advanced.html#custom-iterators
	func _iter_init(iter):
		iter[0] = head
		return iter[0] != null

	func _iter_next(iter):
		iter[0] = iter[0].next
		return iter[0] != null

	func _iter_get(node):
		return node.value


	func count():
		var n = 0
		for value in self:
			n += 1
		return n

	func push(value):
		var node = ListNode.new(value)
		node.next = head
		head = node

	func peek():
		return head.value if head else null

	func pop():
		if not head: return null

		var value = head.value
		head = head.next
		return value

	func to_list():
		var elems = []
		for value in self:
			elems.push_front(value)
		return elems

	func reverse():
		# this is **in-place**
		var reversed = new()
		for value in self:
			reversed.push(value)
		head = reversed.head
