const ALPHABET = "abcdefghijklmnopqrstuvwxyz"

const A = ord("a")


class Cipher:
	var key = null

	func _init(key = null):
		var _random_letter = func(i):
			return ALPHABET[randi_range(0, len(ALPHABET) - 1)]

		self.key = key if key else "".join(range(100).map(_random_letter))

	func encode(plaintext): return _encipher(plaintext, 1)
	func decode(ciphertext): return _encipher(ciphertext, -1)

	func _encipher(text, direction):
		while len(key) < len(text):
			key += key

		return "".join(range(len(text)) \
					.map(func(i): return _enc(text[i], key[i], direction)))

	func _enc(t, k, dir):
		var idx = posmod((ord(t) - A) + dir * (ord(k) - A), len(ALPHABET))
		return ALPHABET[idx]
