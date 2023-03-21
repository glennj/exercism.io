package prime

func Factors(n int64) []int64 {
    //return oneLoopFactors(n)
    //return closureFactors(n)
    return bitshiftFactors(n)
}

// ------------------------------------------------------------
func oneLoopFactors(n int64) (factors []int64) {
	var f int64 = 2

	for f*f <= n {
		if n%f == 0 {
			factors = append(factors, f)
			n /= f
		} else {
			//f++
			if f == 2 {
				f += 1
			} else {
				f += 2
			}
		}
	}

	if n > 1 {
		factors = append(factors, n)
	}

	return factors
}

// ------------------------------------------------------------
func closureFactors(n int64) (factors []int64) {
    addFactors := func(f int64) {
        for n % f == 0 {
            factors = append(factors, f)
            n = n / f
        }
    }

    addFactors(2)

    for f := int64(3); f*f <= n; f += 2 {
        addFactors(f)
	}

	if n > 1 {
		factors = append(factors, n)
	}

	return factors
}

// ------------------------------------------------------------
func bitshiftFactors(n int64) (factors []int64) {
    var f int64

    f = 2
    for n % f == 0 {
        factors = append(factors, f)
        n >>= 1
    }

    for f = 3; f*f <= n; f += 2 {
        for n % f == 0 {
            factors = append(factors, f)
            n = n / f
        }
	}

	if n > 1 {
		factors = append(factors, n)
	}

	return factors
}


/* bench
 *
 * oneloop: incrementing f by 1
 * BenchmarkPrimeFactors-2            15121             81946 ns/op             424 B/op         26 allocs/op
 *
 * oneloop: incrementing f by 2
 * BenchmarkPrimeFactors-2            28616             40576 ns/op             424 B/op         26 allocs/op
 *
 * using a closure is a touch quicker, removing one if statement
 * BenchmarkPrimeFactors-2            32434             36508 ns/op             424 B/op         26 allocs/op
 *
 * using bit shift instead of divide by 2 gives no advantage
 * BenchmarkPrimeFactors-2            31608             36561 ns/op             424 B/op         26 allocs/op
 */
