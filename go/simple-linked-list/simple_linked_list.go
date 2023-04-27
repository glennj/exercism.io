package linkedlist

import "errors"

var ErrEmpty = errors.New("list is empty")

type List struct {
	head *Element
	size int
}

type Element struct {
	value int
	next  *Element
}

func New(elements []int) *List {
	lst := &List{}
	for _, elem := range elements {
		lst.Push(elem)
	}
	return lst
}

// ForEach is an "iterator" to hide the `for` loop
func (l *List) ForEach(fn func(*Element)) {
	for node := l.head; node != nil; node = node.next {
		fn(node)
	}
}

/*
	func (l *List) Size() (size int) {
		l.ForEach(func(_ *Element) { size++ })
		return
	}
*/
func (l *List) Size() int {
	return l.size
}

func (l *List) Push(element int) {
	/*
		node := Element{value: element, next: l.head}
		l.head = &node
	*/
	// same mem allocations, but about 10% quicker
	l.head = &Element{value: element, next: l.head}
	l.size += 1
}

func (l *List) Pop() (int, error) {
	node := l.head
	if node == nil {
		return 0, ErrEmpty
	}
	l.head = node.next
	node.next = nil // helps garbage collection?
	l.size -= 1
	return node.value, nil
}

func (l *List) Array() []int {
	a := make([]int, l.Size())
	i := len(a) - 1
	l.ForEach(func(node *Element) {
		a[i] = node.value
		i--
	})
	return a
}

func (l *List) Reverse() (rev *List) {
	rev = New(nil)
	l.ForEach(func(node *Element) { rev.Push(node.value) })
	return
}

/* bench
 *
 * goos: linux
 * goarch: amd64
 * pkg: linkedlist
 * cpu: AMD EPYC 7542 32-Core Processor
 * BenchmarkNewList         2166705               495.0 ns/op           160 B/op         10 allocs/op
 * BenchmarkListSize       1000000000               0.8085 ns/op          0 B/op          0 allocs/op
 * BenchmarkListPush          24205             50815 ns/op           16000 B/op       1000 allocs/op
 * BenchmarkListPop          240573              4906 ns/op               0 B/op          0 allocs/op
 * BenchmarkListToArray    13081515                93.43 ns/op           80 B/op          1 allocs/op
 * BenchmarkListReverse     1860547               576.2 ns/op           176 B/op         11 allocs/op
 */
