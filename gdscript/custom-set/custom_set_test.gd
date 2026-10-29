func test_sets_with_no_elements_are_empty(solution_script):
	var set = solution_script.Set.new([])
	var got = set.empty()
	var want = true
	return [got, want]


func test_sets_with_elements_are_not_empty(solution_script):
	var set = solution_script.Set.new([1])
	var got = set.empty()
	var want = false
	return [got, want]


func test_nothing_is_contained_in_an_empty_set(solution_script):
	var set = solution_script.Set.new([])
	var got = set.contains(1)
	var want = false
	return [got, want]


func test_when_the_element_is_in_the_set(solution_script):
	var set = solution_script.Set.new([1, 2, 3])
	var got = set.contains(1)
	var want = true
	return [got, want]


func test_when_the_element_is_not_in_the_set(solution_script):
	var set = solution_script.Set.new([1, 2, 3])
	var got = set.contains(4)
	var want = false
	return [got, want]


func test_empty_set_is_a_subset_of_another_empty_set(solution_script):
	var set_one = solution_script.Set.new([])
	var set_two = solution_script.Set.new([])
	var got = set_one.subset(set_two)
	var want = true
	return [got, want]


func test_empty_set_is_a_subset_of_non_empty_set(solution_script):
	var set_one = solution_script.Set.new([])
	var set_two = solution_script.Set.new([1])
	var got = set_one.subset(set_two)
	var want = true
	return [got, want]


func test_non_empty_set_is_not_a_subset_of_empty_set(solution_script):
	var set_one = solution_script.Set.new([1])
	var set_two = solution_script.Set.new([])
	var got = set_one.subset(set_two)
	var want = false
	return [got, want]


func test_set_is_a_subset_of_set_with_exact_same_elements(solution_script):
	var set_one = solution_script.Set.new([1, 2, 3])
	var set_two = solution_script.Set.new([1, 2, 3])
	var got = set_one.subset(set_two)
	var want = true
	return [got, want]


func test_set_is_a_subset_of_larger_set_with_same_elements(solution_script):
	var set_one = solution_script.Set.new([1, 2, 3])
	var set_two = solution_script.Set.new([4, 1, 2, 3])
	var got = set_one.subset(set_two)
	var want = true
	return [got, want]


func test_set_is_not_a_subset_of_set_that_does_not_contain_its_elements(solution_script):
	var set_one = solution_script.Set.new([1, 2, 3])
	var set_two = solution_script.Set.new([4, 1, 3])
	var got = set_one.subset(set_two)
	var want = false
	return [got, want]


func test_the_empty_set_is_disjoint_with_itself(solution_script):
	var set_one = solution_script.Set.new([])
	var set_two = solution_script.Set.new([])
	var got = set_one.disjoint(set_two)
	var want = true
	return [got, want]


func test_empty_set_is_disjoint_with_non_empty_set(solution_script):
	var set_one = solution_script.Set.new([])
	var set_two = solution_script.Set.new([1])
	var got = set_one.disjoint(set_two)
	var want = true
	return [got, want]


func test_non_empty_set_is_disjoint_with_empty_set(solution_script):
	var set_one = solution_script.Set.new([1])
	var set_two = solution_script.Set.new([])
	var got = set_one.disjoint(set_two)
	var want = true
	return [got, want]


func test_sets_are_not_disjoint_if_they_share_an_element(solution_script):
	var set_one = solution_script.Set.new([1, 2])
	var set_two = solution_script.Set.new([2, 3])
	var got = set_one.disjoint(set_two)
	var want = false
	return [got, want]


func test_sets_are_disjoint_if_they_share_no_elements(solution_script):
	var set_one = solution_script.Set.new([1, 2])
	var set_two = solution_script.Set.new([3, 4])
	var got = set_one.disjoint(set_two)
	var want = true
	return [got, want]


func test_empty_sets_are_equal(solution_script):
	var set_one = solution_script.Set.new([])
	var set_two = solution_script.Set.new([])
	var got = set_one.equal(set_two)
	var want = true
	return [got, want]


func test_empty_set_is_not_equal_to_non_empty_set(solution_script):
	var set_one = solution_script.Set.new([])
	var set_two = solution_script.Set.new([1, 2, 3])
	var got = set_one.equal(set_two)
	var want = false
	return [got, want]


func test_non_empty_set_is_not_equal_to_empty_set(solution_script):
	var set_one = solution_script.Set.new([1, 2, 3])
	var set_two = solution_script.Set.new([])
	var got = set_one.equal(set_two)
	var want = false
	return [got, want]


func test_sets_with_the_same_elements_are_equal(solution_script):
	var set_one = solution_script.Set.new([1, 2])
	var set_two = solution_script.Set.new([2, 1])
	var got = set_one.equal(set_two)
	var want = true
	return [got, want]


func test_sets_with_different_elements_are_not_equal(solution_script):
	var set_one = solution_script.Set.new([1, 2, 3])
	var set_two = solution_script.Set.new([1, 2, 4])
	var got = set_one.equal(set_two)
	var want = false
	return [got, want]


func test_set_is_not_equal_to_larger_set_with_same_elements(solution_script):
	var set_one = solution_script.Set.new([1, 2, 3])
	var set_two = solution_script.Set.new([1, 2, 3, 4])
	var got = set_one.equal(set_two)
	var want = false
	return [got, want]


func test_set_is_equal_to_a_set_constructed_from_an_array_with_duplicates(solution_script):
	var set_one = solution_script.Set.new([1])
	var set_two = solution_script.Set.new([1, 1])
	var got = set_one.equal(set_two)
	var want = true
	return [got, want]


func test_add_to_empty_set(solution_script):
	var set = solution_script.Set.new([])
	var got = set.add(3)
	var want = solution_script.Set.new([3])
	return [got.equal(want), true]


func test_add_to_non_empty_set(solution_script):
	var set = solution_script.Set.new([1, 2, 4])
	var got = set.add(3)
	var want = solution_script.Set.new([1, 2, 3, 4])
	return [got.equal(want), true]


func test_adding_an_existing_element_does_not_change_the_set(solution_script):
	var set = solution_script.Set.new([1, 2, 3])
	var got = set.add(3)
	var want = solution_script.Set.new([1, 2, 3])
	return [got.equal(want), true]


func test_intersection_of_two_empty_sets_is_an_empty_set(solution_script):
	var set_one = solution_script.Set.new([])
	var set_two = solution_script.Set.new([])
	var got = set_one.intersection(set_two)
	var want = solution_script.Set.new([])
	return [got.equal(want), true]


func test_intersection_of_an_empty_set_and_non_empty_set_is_an_empty_set(solution_script):
	var set_one = solution_script.Set.new([])
	var set_two = solution_script.Set.new([3, 2, 5])
	var got = set_one.intersection(set_two)
	var want = solution_script.Set.new([])
	return [got.equal(want), true]


func test_intersection_of_a_non_empty_set_and_an_empty_set_is_an_empty_set(solution_script):
	var set_one = solution_script.Set.new([1, 2, 3, 4])
	var set_two = solution_script.Set.new([])
	var got = set_one.intersection(set_two)
	var want = solution_script.Set.new([])
	return [got.equal(want), true]


func test_intersection_of_two_sets_with_no_shared_elements_is_an_empty_set(solution_script):
	var set_one = solution_script.Set.new([1, 2, 3])
	var set_two = solution_script.Set.new([4, 5, 6])
	var got = set_one.intersection(set_two)
	var want = solution_script.Set.new([])
	return [got.equal(want), true]


func test_intersection_of_two_sets_with_shared_elements_is_a_set_of_the_shared_elements(solution_script):
	var set_one = solution_script.Set.new([1, 2, 3, 4])
	var set_two = solution_script.Set.new([3, 2, 5])
	var got = set_one.intersection(set_two)
	var want = solution_script.Set.new([2, 3])
	return [got.equal(want), true]


func test_difference_of_two_empty_sets_is_an_empty_set(solution_script):
	var set_one = solution_script.Set.new([])
	var set_two = solution_script.Set.new([])
	var got = set_one.difference(set_two)
	var want = solution_script.Set.new([])
	return [got.equal(want), true]


func test_difference_of_empty_set_and_non_empty_set_is_an_empty_set(solution_script):
	var set_one = solution_script.Set.new([])
	var set_two = solution_script.Set.new([3, 2, 5])
	var got = set_one.difference(set_two)
	var want = solution_script.Set.new([])
	return [got.equal(want), true]


func test_difference_of_a_non_empty_set_and_an_empty_set_is_the_non_empty_set(solution_script):
	var set_one = solution_script.Set.new([1, 2, 3, 4])
	var set_two = solution_script.Set.new([])
	var got = set_one.difference(set_two)
	var want = solution_script.Set.new([1, 2, 3, 4])
	return [got.equal(want), true]


func test_difference_of_two_non_empty_sets_is_a_set_of_elements_that_are_only_in_the_first_set(solution_script):
	var set_one = solution_script.Set.new([3, 2, 1])
	var set_two = solution_script.Set.new([2, 4])
	var got = set_one.difference(set_two)
	var want = solution_script.Set.new([1, 3])
	return [got.equal(want), true]


func test_difference_removes_all_duplicates_in_the_first_set(solution_script):
	var set_one = solution_script.Set.new([1, 1])
	var set_two = solution_script.Set.new([1])
	var got = set_one.difference(set_two)
	var want = solution_script.Set.new([])
	return [got.equal(want), true]


func test_union_of_empty_sets_is_an_empty_set(solution_script):
	var set_one = solution_script.Set.new([])
	var set_two = solution_script.Set.new([])
	var got = set_one.union(set_two)
	var want = solution_script.Set.new([])
	return [got.equal(want), true]


func test_union_of_an_empty_set_and_non_empty_set_is_the_non_empty_set(solution_script):
	var set_one = solution_script.Set.new([])
	var set_two = solution_script.Set.new([2])
	var got = set_one.union(set_two)
	var want = solution_script.Set.new([2])
	return [got.equal(want), true]


func test_union_of_a_non_empty_set_and_empty_set_is_the_non_empty_set(solution_script):
	var set_one = solution_script.Set.new([1, 3])
	var set_two = solution_script.Set.new([])
	var got = set_one.union(set_two)
	var want = solution_script.Set.new([1, 3])
	return [got.equal(want), true]


func test_union_of_non_empty_sets_contains_all_unique_elements(solution_script):
	var set_one = solution_script.Set.new([1, 3])
	var set_two = solution_script.Set.new([2, 3])
	var got = set_one.union(set_two)
	var want = solution_script.Set.new([3, 2, 1])
	return [got.equal(want), true]
