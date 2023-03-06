package lasagna

// PreparationTime estimates the time needed to prepare the lasangna
func PreparationTime(layers []string, timePerLayer int) int {
	if timePerLayer == 0 {
		timePerLayer = 2
	}
	return len(layers) * timePerLayer
}

const noodleUnits int = 50     // grams
const sauceUnits float64 = 0.2 // litres

// Quantities calculates the quantities of noodles and sauce in the recipe.
func Quantities(layers []string) (noodles int, sauce float64) {
	for i := 0; i < len(layers); i++ {
		switch layers[i] {
		case "noodles":
			noodles += noodleUnits
		case "sauce":
			sauce += sauceUnits
		}
	}
	return
}

// AddSecretIngredient sets my last ingredient to be the same as your last ingredient.
func AddSecretIngredient(yours, mine []string) {
	mine[len(mine)-1] = yours[len(yours)-1]
}

// ScaleRecipe calculates new portion sizes for a different number of portions.
func ScaleRecipe(portionsForTwo []float64, num int) []float64 {
	scaled := make([]float64, 0, len(portionsForTwo))
	for i := 0; i < len(portionsForTwo); i++ {
		portion := portionsForTwo[i] * float64(num) / 2.0
		scaled = append(scaled, portion)
	}
	return scaled
}
