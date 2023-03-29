package stringset

import (
	"fmt"
	"strings"
)

type Set map[string]bool

func New() Set {
	return Set{}
}

func NewFromSlice(l []string) Set {
	set := New()
	for _, s := range l {
		set.Add(s)
	}
	return set
}

func (s Set) String() string {
	res := strings.Builder{}
	sep := ""
	res.WriteByte('{')
	for elem := range s {
		res.WriteString(fmt.Sprintf("%s%q", sep, elem))
		sep = ", "
	}
	res.WriteByte('}')
	return res.String()
}

func (s Set) IsEmpty() bool {
	return len(s) == 0
}

func (s Set) Has(elem string) bool {
	_, exists := s[elem]
	return exists
}

func (s Set) Add(elem string) {
	s[elem] = true
}

func Subset(s1, s2 Set) bool {
	if len(s1) > len(s2) {
		return false
	}
	for elem := range s1 {
		if !s2.Has(elem) {
			return false
		}
	}
	return true
}

func Disjoint(s1, s2 Set) bool {
	for elem := range s1 {
		if s2.Has(elem) {
			return false
		}
	}
	return true
}

func Equal(s1, s2 Set) bool {
	return len(s1) == len(s2) && Subset(s1, s2)
}

func Intersection(s1, s2 Set) Set {
	intersect := New()
	for elem := range s1 {
		if s2.Has(elem) {
			intersect.Add(elem)
		}
	}
	return intersect
}

func Difference(s1, s2 Set) Set {
	diff := New()
	for elem := range s1 {
		if !s2.Has(elem) {
			diff.Add(elem)
		}
	}
	return diff
}

func Union(s1, s2 Set) Set {
	union := New()
	for elem := range s1 {
		union.Add(elem)
	}
	for elem := range Difference(s2, s1) {
		union.Add(elem)
	}
	return union
}

/* bench
 * BenchmarkNewFromSlice1e1 	 5092887	       254.9 ns/op	       0 B/op	       0 allocs/op
 * BenchmarkNewFromSlice1e2 	  104722	     10420 ns/op	    5364 B/op	       7 allocs/op
 * BenchmarkNewFromSlice1e3 	   13112	     93634 ns/op	   45319 B/op	      22 allocs/op
 * BenchmarkNewFromSlice1e4 	    1308	    948322 ns/op	  374183 B/op	     135 allocs/op
 */
