func rebase(input_base, digits, output_base):
	if input_base < 2 or output_base < 2: return null

	var decimal = 0
	for digit in digits:
		if digit < 0 or digit >= input_base: return null
		decimal = decimal * input_base + digit

	if decimal == 0: return [0]

	var out_digits = []
	while decimal > 0:
		out_digits.push_front(decimal % output_base)
		decimal /= output_base

	return out_digits
