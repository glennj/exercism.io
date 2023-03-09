package linkedlist

// TODO when checking for empty list and pop/unshift
// import "errors"

type Node struct {
	Value interface{}
	next  *Node
	prev  *Node
}

type List struct {
	head *Node
	tail *Node
}

func NewList(args ...interface{}) *List {
	lst := List{}
	for _, val := range args {
		lst.Push(val)
	}
	return &lst
}

func (n *Node) Next() *Node {
	return n.next
}

func (n *Node) Prev() *Node {
	return n.prev
}

func (l *List) Unshift(v interface{}) {
	n := Node{Value: v}
	if l.head == nil {
		l.head = &n
		l.tail = &n
	} else {
		l.head.prev = &n
		n.next = l.head
		l.head = &n
	}
}

func (l *List) Shift() (interface{}, error) {
	// TODO check for empty list
	n := l.head
	if n.next == nil {
		l.head = nil
		l.tail = nil
	} else {
		l.head = n.next
		l.head.prev = nil
		n.next = nil // helpful for garbage collection?
	}
	return n.Value, nil
}

func (l *List) Push(v interface{}) {
	n := Node{Value: v}
	if l.tail == nil {
		l.head = &n
		l.tail = &n
	} else {
		l.tail.next = &n
		n.prev = l.tail
		l.tail = &n
	}
}

func (l *List) Pop() (interface{}, error) {
	// TODO check for empty list
	n := l.tail
	if n.prev == nil {
		l.head = nil
		l.tail = nil
	} else {
		l.tail = n.prev
		l.tail.next = nil
		n.prev = nil // helpful for garbage collection?
	}
	return n.Value, nil
}

func (l *List) Reverse() {
	newList := List{}
	for n := l.head; n != nil; n = n.next {
		newList.Unshift(n.Value)
	}
	l.head = newList.head
	l.tail = newList.tail
}

func (l *List) First() *Node {
	return l.head
}

func (l *List) Last() *Node {
	return l.tail
}
