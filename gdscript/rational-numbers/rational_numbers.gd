class Rational:
	@export var numer: int
	@export var denom: int

	func _init(numer, denom):
		if denom < 0:
			numer *= -1
			denom *= -1

		var g = gcd(numer, denom)
		numer /= g
		denom /= g

		self.numer = numer
		self.denom = denom

	func gcd(a, b):
		return a if b == 0 else gcd(b, posmod(a, b))

	func add(other):
		return new(numer * other.denom + other.numer * denom, denom * other.denom)

	func negated():
		return new(-numer, denom)

	func sub(other):
		return add(other.negated())

	func mul(other):
		return new(numer * other.numer, denom * other.denom)

	func invert():
		return new(denom, numer)

	func div(other):
		return mul(other.invert())

	func abs():
		return new(abs(numer), denom)

	func exprational(power):
		var r = self
		if power < 0:
			power = -power
			r = invert()
		return new(r.numer ** power, r.denom ** power)

	func expreal(base):
		return base ** (1.0 * numer / denom)

	func reduce():
		# I'm already reduced
		return self
