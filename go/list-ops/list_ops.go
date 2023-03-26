// Package listops ...
package listops

/* The only builtin functionality used
 * - iterating over a slice with `range`
 * - get/set a slice element with bracket syntax
 * - `make` to create the appropriate size slice
 *
 * NOT using: `len`, `append`
 */

// IntList is a list of ints
type IntList []int

// Foldl is a "left fold"
func (s IntList) Foldl(fn func(int, int) int, initial int) int {
	result := initial
	for _, n := range s {
		result = fn(result, n)
	}
	return result
}

// Length _can_ be implemented with a Foldl call, but more efficient is:
func (s IntList) Length() (length int) {
	for range s {
		length++
	}
	return
}

// Foldr _can_ be implemented with Reverse |> Foldl, but more efficient is:
func (s IntList) Foldr(fn func(int, int) int, initial int) int {
	result := initial
	for i := s.Length() - 1; i >= 0; i-- {
		result = fn(s[i], result)
	}
	return result
}

// Reverse returns a new list with this list's elements in reverse order
func (s IntList) Reverse() IntList {
	size := s.Length()
	result := make(IntList, size)
	for i, j := 0, size-1; i < size; i, j = i+1, j-1 {
		result[i] = s[j]
	}
	return result
}

// Filter returns a new list containing the elememts of this list that "pass" the function argument.
func (s IntList) Filter(fn func(int) bool) IntList {
	/*
		result := make(IntList, s.Length())
		idx := 0
		for _, n := range s {
			if fn(n) {
				result[idx] = n
				idx++
			}
		}
		return result[:idx]
	*/

	/* Alternate implementation: since the final length is unknown
	 * maintain a list of indexes to keep from the `s` list.
	 * This doesn't help much for this exercise, but if we're
	 * dealing with a list of larger structures, allocating
	 * a list of ints will require less space, and then we'll
	 * know the actual size of the resulting list.
	 */

	keepIdxs := make([]int, s.Length())
	idx := 0
	for i, n := range s {
		if fn(n) {
			keepIdxs[idx] = i
			idx++
		}
	}

	result := make(IntList, idx)
	for i, j := range keepIdxs[:idx] {
		result[i] = s[j]
	}
	return result

}

// Map returns a new list containing the results of applying the func to this list's elements in turn.
func (s IntList) Map(fn func(int) int) IntList {
	result := make(IntList, s.Length())
	for i, n := range s {
		result[i] = fn(n)
	}
	return result
}

// Append returns a new list with this list's elements and the other list's elements
func (s IntList) Append(lst IntList) IntList {
	return s.Concat([]IntList{lst})
}

// Concat returns a new list containing all the elements of this list and the arg lists
func (s IntList) Concat(lists []IntList) IntList {
	size := s.Length()
	for _, lst := range lists {
		size += lst.Length()
	}

	result := make(IntList, size)
	i := 0

	appendList := func(lst IntList) {
		for _, n := range lst {
			result[i] = n
			i++
		}
	}

	appendList(s)
	for _, lst := range lists {
		appendList(lst)
	}
	return result
}
