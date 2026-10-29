class LinkedList:

	class ListNode:
		var next: ListNode
		var prev: ListNode
		var value: Variant

		func _init(val):
			value = val

	# ----------------------------------------
	var head: ListNode
	var tail: ListNode

	func push(value):
		var node = ListNode.new(value)
		if tail:
			tail.next = node
			node.prev = tail
		else:
			head = node
		tail = node

	func pop():
		var val
		var node = tail

		if node:
			val = node.value
			if node.prev:
				node.prev.next = null
				tail = node.prev
			else:
				tail = null
				head = null

		return val

	func unshift(value):
		var node = ListNode.new(value)
		if head:
			head.prev = node
			node.next = head
		else:
			tail = node
		head = node

	func shift():
		var val
		var node = head

		if node:
			val = node.value
			if node.next:
				node.next.prev = null
				head = node.next
			else:
				tail = null
				head = null

		return val

	func delete(value):
		var node = head
		while node:
			if node.value != value:
				node = node.next
				continue
			
			# unlink this node
			if node.next:
				node.next.prev = node.prev
			else:
				tail = node.prev

			if node.prev:
				node.prev.next = node.next
			else:
				head = node.next

			return


	func count():
		var count = 0
		var node = head
		while node:
			count += 1
			node = node.next
		return count
