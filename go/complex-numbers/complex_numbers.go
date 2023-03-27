package complexnumbers

import "math"

type Number struct {
	r float64
	i float64
}

func (n Number) Real() float64 {
	return n.r
}

func (n Number) Imaginary() float64 {
	return n.i
}

func (n Number) parts() (float64, float64) {
	return n.Real(), n.Imaginary()
}

func (n Number) Add(o Number) Number {
	a, b := n.parts()
	c, d := o.parts()
	return Number{a + c, b + d}
}

func (n Number) Subtract(o Number) Number {
	a, b := n.parts()
	c, d := o.parts()
	return Number{a - c, b - d}
}

func (n Number) Multiply(o Number) Number {
	a, b := n.parts()
	c, d := o.parts()
	return Number{
		r: a*c - b*d,
		i: b*c + a*d,
	}
}

func (n Number) Times(factor float64) Number {
	a, b := n.parts()
	return Number{a * factor, b * factor}
}

func (n Number) Divide(o Number) Number {
	a, b := n.parts()
	c, d := o.parts()
	div := c*c + d*d
	return Number{
		r: (a*c + b*d) / div,
		i: (b*c - a*d) / div,
	}
}

func (n Number) Conjugate() Number {
	a, b := n.parts()
	return Number{a, -b}
}

func (n Number) Abs() float64 {
	a, b := n.parts()
	return math.Hypot(a, b)
}

func (n Number) Exp() Number {
	a, b := n.parts()
	realPart := math.Pow(math.E, a)
	imagPart := Number{math.Cos(b), math.Sin(b)}
	return imagPart.Times(realPart)
}
