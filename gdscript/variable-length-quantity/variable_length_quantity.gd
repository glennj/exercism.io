const SHIFT_AMT = 7
const MSB  = 0b1000_0000
const MASK = 0b0111_1111


func encode(numbers: Array):
	var bytes = []
	for i in range(numbers.size() - 1, -1, -1):
		var val = numbers[i]
		var msb = 0
		while true:
			var byte = (val & MASK) | msb
			bytes.push_front(byte)
			msb = MSB
			val >>= SHIFT_AMT
			if val == 0: break # this is a do-while loop
	return bytes


func decode(encoded: Array):
	# check for incomplete sequence
	if encoded[-1] & MSB != 0: return null
	
	var values = []
	var n = 0
	for byte in encoded:
		n = (n << SHIFT_AMT) + (byte & MASK)
		if byte & MSB == 0:
			values.append(n)
			n = 0
	return values

