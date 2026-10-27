const ALLERGENS = [
	"eggs", "peanuts", "shellfish", "strawberries",
	"tomatoes", "chocolate", "pollen", "cats",
]


@export var score: int
@export var lst: Array:
	get = _get_lst


func allergic_to(item):
	var idx = ALLERGENS.find(item)
	return (score >> idx) & 1 == 1


func _get_lst():
	return ALLERGENS.filter(allergic_to)
