package allyourbase

import "errors"

var (
	ErrInputBase       = errors.New("input base must be >= 2")
	ErrOutputBase      = errors.New("output base must be >= 2")
	ErrDigitOutOfRange = errors.New("all digits must satisfy 0 <= d < input base")
)

func ConvertToBase(inputBase int, inputDigits []int, outputBase int) ([]int, error) {

	if inputBase < 2 {
		return nil, ErrInputBase
	}
	if outputBase < 2 {
		return nil, ErrOutputBase
	}

	decimal, err := inputToDecimal(inputBase, inputDigits)
	if err != nil {
		return nil, err
	}

	return decimalToOutput(outputBase, decimal), nil
}

// ------------------------------------------------------------
func inputToDecimal(inputBase int, inputDigits []int) (int, error) {
	var decimal int

	for _, digit := range inputDigits {
		if digit < 0 || digit >= inputBase {
			return 0, ErrDigitOutOfRange
		}
		decimal = decimal*inputBase + digit
	}

	return decimal, nil
}

// ------------------------------------------------------------
func decimalToOutput(outputBase int, decimal int) []int {
	var outputDigits []int

	switch decimal {
	case 0:
		outputDigits = []int{0}
	default:
		for decimal > 0 {
			rem := decimal % outputBase
			outputDigits = append(outputDigits, rem)
			decimal = (decimal - rem) / outputBase
		}
		reverse(outputDigits)
	}

	return outputDigits
}

// ------------------------------------------------------------
func reverse(array []int) {
	for i, j := 0, len(array)-1; i < j; i, j = i+1, j-1 {
		array[i], array[j] = array[j], array[i]
	}
}
