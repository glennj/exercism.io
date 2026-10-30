func equal(a, b):
	if a == null and b == null:
		return true
	if a == null or b == null:
		return false
	return a.datum == b.datum and equal(a.left, b.left) and equal(a.right, b.right)


func test_data_is_retained(solution_script):
	var bst = solution_script.BinarySearchTree.new([4])
	var got = bst.data()
	var want = solution_script.TreeNode.new(4, null, null)
	return [equal(got, want), true]


func test_smaller_number_at_left_node(solution_script):
	var bst = solution_script.BinarySearchTree.new([4, 2])
	var got = bst.data()
	var want = solution_script.TreeNode.new(4, solution_script.TreeNode.new(2, null, null), null)
	return [equal(got, want), true]


func test_same_number_at_left_node(solution_script):
	var bst = solution_script.BinarySearchTree.new([4, 4])
	var got = bst.data()
	var want = solution_script.TreeNode.new(4, solution_script.TreeNode.new(4, null, null), null)
	return [equal(got, want), true]


func test_greater_number_at_right_node(solution_script):
	var bst = solution_script.BinarySearchTree.new([4, 5])
	var got = bst.data()
	var want = solution_script.TreeNode.new(4, null, solution_script.TreeNode.new(5, null, null))
	return [equal(got, want), true]


func test_can_create_complex_tree(solution_script):
	var bst = solution_script.BinarySearchTree.new([4, 2, 6, 1, 3, 5, 7])
	var got = bst.data()
	var want = solution_script.TreeNode.new(4, solution_script.TreeNode.new(2, solution_script.TreeNode.new(1, null, null), solution_script.TreeNode.new(3, null, null)), solution_script.TreeNode.new(6, solution_script.TreeNode.new(5, null, null), solution_script.TreeNode.new(7, null, null)))
	return [equal(got, want), true]


func test_can_sort_single_number(solution_script):
	var bst = solution_script.BinarySearchTree.new([2])
	var got = bst.sorted_data()
	var want = [2]
	return [got, want]


func test_can_sort_if_second_number_is_smaller_than_first(solution_script):
	var bst = solution_script.BinarySearchTree.new([2, 1])
	var got = bst.sorted_data()
	var want = [1, 2]
	return [got, want]


func test_can_sort_if_second_number_is_same_as_first(solution_script):
	var bst = solution_script.BinarySearchTree.new([2, 2])
	var got = bst.sorted_data()
	var want = [2, 2]
	return [got, want]


func test_can_sort_if_second_number_is_greater_than_first(solution_script):
	var bst = solution_script.BinarySearchTree.new([2, 3])
	var got = bst.sorted_data()
	var want = [2, 3]
	return [got, want]


func test_can_sort_complex_tree(solution_script):
	var bst = solution_script.BinarySearchTree.new([2, 1, 3, 6, 7, 5])
	var got = bst.sorted_data()
	var want = [1, 2, 3, 5, 6, 7]
	return [got, want]
