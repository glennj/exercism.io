func _sorted(array):
	# Array.sort() sorts an array in place and returns null.
	# This helper method allows us to sort and compare in the same line.
	array.sort()
	return array



func test_eggs_not_allergic_to_anything(allergies):
	allergies.score = 0
	return [allergies.allergic_to("eggs"), false]


func test_eggs_allergic_only_to_eggs(allergies):
	allergies.score = 1
	return [allergies.allergic_to("eggs"), true]


func test_eggs_allergic_to_eggs_and_something_else(allergies):
	allergies.score = 3
	return [allergies.allergic_to("eggs"), true]


func test_eggs_allergic_to_something_but_not_eggs(allergies):
	allergies.score = 2
	return [allergies.allergic_to("eggs"), false]


func test_eggs_allergic_to_everything(allergies):
	allergies.score = 255
	return [allergies.allergic_to("eggs"), true]


func test_peanuts_not_allergic_to_anything(allergies):
	allergies.score = 0
	return [allergies.allergic_to("peanuts"), false]


func test_peanuts_allergic_only_to_peanuts(allergies):
	allergies.score = 2
	return [allergies.allergic_to("peanuts"), true]


func test_peanuts_allergic_to_peanuts_and_something_else(allergies):
	allergies.score = 7
	return [allergies.allergic_to("peanuts"), true]


func test_peanuts_allergic_to_something_but_not_peanuts(allergies):
	allergies.score = 5
	return [allergies.allergic_to("peanuts"), false]


func test_peanuts_allergic_to_everything(allergies):
	allergies.score = 255
	return [allergies.allergic_to("peanuts"), true]


func test_shellfish_not_allergic_to_anything(allergies):
	allergies.score = 0
	return [allergies.allergic_to("shellfish"), false]


func test_shellfish_allergic_only_to_shellfish(allergies):
	allergies.score = 4
	return [allergies.allergic_to("shellfish"), true]


func test_shellfish_allergic_to_shellfish_and_something_else(allergies):
	allergies.score = 14
	return [allergies.allergic_to("shellfish"), true]


func test_shellfish_allergic_to_something_but_not_shellfish(allergies):
	allergies.score = 10
	return [allergies.allergic_to("shellfish"), false]


func test_shellfish_allergic_to_everything(allergies):
	allergies.score = 255
	return [allergies.allergic_to("shellfish"), true]


func test_strawberries_not_allergic_to_anything(allergies):
	allergies.score = 0
	return [allergies.allergic_to("strawberries"), false]


func test_strawberries_allergic_only_to_strawberries(allergies):
	allergies.score = 8
	return [allergies.allergic_to("strawberries"), true]


func test_strawberries_allergic_to_strawberries_and_something_else(allergies):
	allergies.score = 28
	return [allergies.allergic_to("strawberries"), true]


func test_strawberries_allergic_to_something_but_not_strawberries(allergies):
	allergies.score = 20
	return [allergies.allergic_to("strawberries"), false]


func test_strawberries_allergic_to_everything(allergies):
	allergies.score = 255
	return [allergies.allergic_to("strawberries"), true]


func test_tomatoes_not_allergic_to_anything(allergies):
	allergies.score = 0
	return [allergies.allergic_to("tomatoes"), false]


func test_tomatoes_allergic_only_to_tomatoes(allergies):
	allergies.score = 16
	return [allergies.allergic_to("tomatoes"), true]


func test_tomatoes_allergic_to_tomatoes_and_something_else(allergies):
	allergies.score = 56
	return [allergies.allergic_to("tomatoes"), true]


func test_tomatoes_allergic_to_something_but_not_tomatoes(allergies):
	allergies.score = 40
	return [allergies.allergic_to("tomatoes"), false]


func test_tomatoes_allergic_to_everything(allergies):
	allergies.score = 255
	return [allergies.allergic_to("tomatoes"), true]


func test_chocolate_not_allergic_to_anything(allergies):
	allergies.score = 0
	return [allergies.allergic_to("chocolate"), false]


func test_chocolate_allergic_only_to_chocolate(allergies):
	allergies.score = 32
	return [allergies.allergic_to("chocolate"), true]


func test_chocolate_allergic_to_chocolate_and_something_else(allergies):
	allergies.score = 112
	return [allergies.allergic_to("chocolate"), true]


func test_chocolate_allergic_to_something_but_not_chocolate(allergies):
	allergies.score = 80
	return [allergies.allergic_to("chocolate"), false]


func test_chocolate_allergic_to_everything(allergies):
	allergies.score = 255
	return [allergies.allergic_to("chocolate"), true]


func test_pollen_not_allergic_to_anything(allergies):
	allergies.score = 0
	return [allergies.allergic_to("pollen"), false]


func test_pollen_allergic_only_to_pollen(allergies):
	allergies.score = 64
	return [allergies.allergic_to("pollen"), true]


func test_pollen_allergic_to_pollen_and_something_else(allergies):
	allergies.score = 224
	return [allergies.allergic_to("pollen"), true]


func test_pollen_allergic_to_something_but_not_pollen(allergies):
	allergies.score = 160
	return [allergies.allergic_to("pollen"), false]


func test_pollen_allergic_to_everything(allergies):
	allergies.score = 255
	return [allergies.allergic_to("pollen"), true]


func test_cats_not_allergic_to_anything(allergies):
	allergies.score = 0
	return [allergies.allergic_to("cats"), false]


func test_cats_allergic_only_to_cats(allergies):
	allergies.score = 128
	return [allergies.allergic_to("cats"), true]


func test_cats_allergic_to_cats_and_something_else(allergies):
	allergies.score = 192
	return [allergies.allergic_to("cats"), true]


func test_cats_allergic_to_something_but_not_cats(allergies):
	allergies.score = 64
	return [allergies.allergic_to("cats"), false]


func test_cats_allergic_to_everything(allergies):
	allergies.score = 255
	return [allergies.allergic_to("cats"), true]


func test_list_when_no_allergies(allergies):
	allergies.score = 0
	return [
		_sorted(allergies.lst),
		_sorted([])
	]


func test_list_when_just_eggs(allergies):
	allergies.score = 1
	return [
		_sorted(allergies.lst),
		_sorted(['eggs'])
	]


func test_list_when_just_peanuts(allergies):
	allergies.score = 2
	return [
		_sorted(allergies.lst),
		_sorted(['peanuts'])
	]


func test_list_when_just_strawberries(allergies):
	allergies.score = 8
	return [
		_sorted(allergies.lst),
		_sorted(['strawberries'])
	]


func test_list_when_eggs_and_peanuts(allergies):
	allergies.score = 3
	return [
		_sorted(allergies.lst),
		_sorted(['eggs', 'peanuts'])
	]


func test_list_when_more_than_eggs_but_not_peanuts(allergies):
	allergies.score = 5
	return [
		_sorted(allergies.lst),
		_sorted(['eggs', 'shellfish'])
	]


func test_list_when_lots_of_stuff(allergies):
	allergies.score = 248
	return [
		_sorted(allergies.lst),
		_sorted(['strawberries', 'tomatoes', 'chocolate', 'pollen', 'cats'])
	]


func test_list_when_everything(allergies):
	allergies.score = 255
	return [
		_sorted(allergies.lst),
		_sorted(['eggs', 'peanuts', 'shellfish', 'strawberries', 'tomatoes', 'chocolate', 'pollen', 'cats'])
	]


func test_list_when_no_allergen_score_parts(allergies):
	allergies.score = 509
	return [
		_sorted(allergies.lst),
		_sorted(['eggs', 'shellfish', 'strawberries', 'tomatoes', 'chocolate', 'pollen', 'cats'])
	]


func test_list_when_no_allergen_score_parts_without_highest_valid_score(allergies):
	allergies.score = 257
	return [
		_sorted(allergies.lst),
		_sorted(['eggs'])
	]
