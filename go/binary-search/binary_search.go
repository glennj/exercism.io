package binarysearch

func SearchInts(list []int, key int) int {
	return SearchIntsIterative(list, key)
	//return SearchIntsRecursive(list, key)
}

func SearchIntsIterative(list []int, key int) int {
	i := 0
	j := len(list) - 1

	for i <= j {
		//mid := (i + j) / 2
		mid := (i + j) >> 1
		switch {
		case list[mid] > key:
			j = mid - 1
		case list[mid] < key:
			i = mid + 1
		default:
			return mid
		}
	}

	return -1
}

func SearchIntsRecursive(list []int, key int) int {
	return searchRecursively(list, key, 0, len(list)-1)
}

func searchRecursively(list []int, key, i, j int) int {
	if i > j {
		return -1
	}

	mid := (i + j) >> 1

	switch {
	case list[mid] > key:
		return searchRecursively(list, key, i, mid-1)
	case list[mid] < key:
		return searchRecursively(list, key, mid+1, j)
	default:
		return mid
	}
}

/* interesting benchmarks:
 *
 * Iterative
 *
 * if I use `mid := (i + j) / 2`
 * Benchmark1e2    159819210                8.123 ns/op           0 B/op          0 allocs/op
 * Benchmark1e4    93887647                16.20 ns/op            0 B/op          0 allocs/op
 * Benchmark1e6    41902825                24.39 ns/op            0 B/op          0 allocs/op
 * Benchmark1e8    32599132                37.91 ns/op            0 B/op          0 allocs/op
 *
 * but by right-shifting instead of dividing
 * Benchmark1e2    197842873                8.074 ns/op           0 B/op          0 allocs/op
 * Benchmark1e4    121594132               14.81 ns/op            0 B/op          0 allocs/op
 * Benchmark1e6    44286306                24.61 ns/op            0 B/op          0 allocs/op
 * Benchmark1e8    37707320                29.25 ns/op            0 B/op          0 allocs/op
 *
 * Recursive, 2x slower
 *
 * Benchmark1e2    100000000               16.01 ns/op            0 B/op          0 allocs/op
 * Benchmark1e4    70486605                23.94 ns/op            0 B/op          0 allocs/op
 * Benchmark1e6    31839205                37.33 ns/op            0 B/op          0 allocs/op
 * Benchmark1e8    20417541                58.17 ns/op            0 B/op          0 allocs/op
 */
