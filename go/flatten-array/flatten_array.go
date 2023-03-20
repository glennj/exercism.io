package flatten

import "reflect"

func Flatten(nested interface{}) []interface{} {
	flattened := []interface{}{}

	/*
		if isArray(nested) {
			for _, elem := range nested.([]interface{}) {
				flattened = append(flattened, Flatten(elem)...)
			}
		} else if nested != nil {
			flattened = append(flattened, nested)
		}
	*/

	/*
		switch nested.(type) {
		case []interface{}:
			for _, elem := range nested.([]interface{}) {
				flattened = append(flattened, Flatten(elem)...)
			}
		case nil:
		default:
			flattened = append(flattened, nested)
		}
	*/

	if slice, ok := nested.([]interface{}); ok {
		for _, elem := range slice {
			flattened = append(flattened, Flatten(elem)...)
		}
	} else if nested != nil {
		flattened = append(flattened, nested)
	}

	return flattened
}

// from https://github.com/ahl5esoft/golang-underscore/blob/v1.7.1/is-array.go
func isArray(item interface{}) bool {
	reflected := reflect.ValueOf(item)
	return reflected.Kind() == reflect.Array || reflected.Kind() == reflect.Slice
}

/* benchmarks
 *
 * using reflect module
 * BenchmarkFlatten-2        222837              5145 ns/op            3424 B/op         90 allocs/op
 *
 * using `nested.(type)` switch -- same allocation
 * BenchmarkFlatten-2        202786              5014 ns/op            3424 B/op         90 allocs/op
 *
 * more straightforward type assertion
 * BenchmarkFlatten-2        239878              5004 ns/op            3424 B/op         90 allocs/op
 */
