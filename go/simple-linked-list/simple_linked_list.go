package linkedlist

import "errors"

var ErrEmpty = errors.New("list is empty")

type List struct {
	head *Element
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

func (l *List) Size() (size int) {
	l.ForEach(func(_ *Element) { size++ })
	return
}

func (l *List) Push(element int) {
	/*
		node := Element{value: element, next: l.head}
		l.head = &node
	*/
	// same mem allocations, but about 10% quicker
	l.head = &Element{value: element, next: l.head}
}

func (l *List) Pop() (int, error) {
	node := l.head
	if node == nil {
		return 0, ErrEmpty
	}
	l.head = node.next
	node.next = nil // helps garbage collection?
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
 * BenchmarkNewList         2272036               528.2 ns/op           160 B/op         10 allocs/op
 * BenchmarkListSize       100000000               13.09 ns/op            0 B/op          0 allocs/op
 * BenchmarkListPush          23836             49648 ns/op           16000 B/op       1000 allocs/op
 * BenchmarkListPop          241854              5174 ns/op               0 B/op          0 allocs/op
 * BenchmarkListToArray    10096894               114.6 ns/op            80 B/op          1 allocs/op
 * BenchmarkListReverse     2069779               576.7 ns/op           168 B/op         11 allocs/op
 */
