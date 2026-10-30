class TreeNode:
	var datum = null
	var left: TreeNode
	var right: TreeNode

	func _init(datum, left = null, right = null):
		self.datum = datum
		self.left = left
		self.right = right

	func insert(datum) -> void:
		if datum <= self.datum:
			if left:
				left.insert(datum)
			else:
				left = new(datum)
		else:
			if right:
				right.insert(datum)
			else:
				right = new(datum)

	func inorder(data = []) -> Array:
		if left: 
			left.inorder(data)
		data.append(datum)
		if right:
			right.inorder(data)
		return data


class BinarySearchTree:
	var root: TreeNode

	func _init(data):
		for datum in data:
			if root:
				root.insert(datum)
			else:
				root = TreeNode.new(datum)

	func data() -> TreeNode:
		return root

	func sorted_data() -> Array:
		return root.inorder() if root else []
