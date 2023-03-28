package summultiples

//lint:file-ignore U1000 only one func will actually be used

import "math/big"

func SumMultiples(limit int, divisors ...int) (sum int) {
	//return mapSumMultiples(limit, divisors)
	//return bitSumMultiples(limit, divisors)
	return divSumMultiples(limit, divisors)
}

func mapSumMultiples(limit int, divisors []int) (sum int) {
	//multiples := make(map[int]bool)
	multiples := make(map[int]bool, limit)

	for _, d := range divisors {
		if d >= 1 {
			f := d
			for f < limit {
				multiples[f] = true
				f += d
			}
		}
	}

	for factor := range multiples {
		sum += factor
	}
	return sum
}

// ------------------------------------------------------------
func bitSumMultiples(limit int, divisors []int) (sum int) {
	var bitfield big.Int

	for _, d := range divisors {
		if d >= 1 {
			f := d
			for f < limit {
				bitfield.SetBit(&bitfield, f, 1)
				f += d
			}
		}
	}

	for f := bitfield.BitLen() - 1; f >= 1; f-- {
		if bitfield.Bit(f) == 1 {
			sum += f
		}
	}
	return sum

}

// ------------------------------------------------------------
func divSumMultiples(limit int, divisors []int) (sum int) {
	for n := 1; n < limit; n++ {
		for _, d := range divisors {
			if d > 0 && n%d == 0 {
				sum += n
				break
			}
		}
	}
	return
}

// This is working, but it's really expensive
// BenchmarkSumMultiples        859           1372682 ns/op          487543 B/op        468 allocs/op
//
// with a length: `make(map[int]bool, limit)` it's quicker, but hungrier for memory
// BenchmarkSumMultiples       1674            703186 ns/op          623861 B/op         48 allocs/op
//
// using a bitfield to store the factors is faster and leaner
// BenchmarkSumMultiples       5030            220318 ns/op           44136 B/op         82 allocs/op
//
// the `divSum...` solution where we don't have to store the unique multiples wins for memory
// but I am shocked (shocked I say) that it is slower than the bitfield solution
// BenchmarkSumMultiples       3987            278226 ns/op               0 B/op          0 allocs/op
