package cars

// CalculateWorkingCarsPerHour calculates how many working cars are
// produced by the assembly line every hour.
func CalculateWorkingCarsPerHour(productionRate int, successRate float64) float64 {
	return float64(productionRate) * successRate / 100.0
}

// CalculateWorkingCarsPerMinute calculates how many working cars are
// produced by the assembly line every minute.
func CalculateWorkingCarsPerMinute(productionRate int, successRate float64) int {
	return int(CalculateWorkingCarsPerHour(productionRate, successRate)) / 60
}

// costPerUnitCar: production cost of a single car
const costPerUnitCar = 10000
// costPerTenCars: production cost of ten cars
const costPerTenCars = 95000

// CalculateCost works out the cost of producing the given number of cars.
func CalculateCost(carsCount int) uint {
    totalCost := 0
	totalCost += costPerTenCars * (carsCount / 10)
    totalCost += costPerUnitCar * (carsCount % 10)
	return uint(totalCost)
}
