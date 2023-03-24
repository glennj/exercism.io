package twobucket

import "fmt"

type Bucket struct {
	Name   string
	Size   int
	Amount int
}

func (b *Bucket) String() string {
	return fmt.Sprintf("\"%s\":%d/%d", b.Name, b.Amount, b.Size)
}

func (b *Bucket) IsEmpty() bool {
	return b.Amount == 0
}

func (b *Bucket) IsFull() bool {
	return b.Amount == b.Size
}

func (b *Bucket) Fill() {
	b.Amount = b.Size
}

func (b *Bucket) Empty() {
	b.Amount = 0
}

func (b *Bucket) Capacity() int {
	return b.Size - b.Amount
}

func (b *Bucket) PourInto(other *Bucket) {
	qty := other.Capacity()
	if b.Amount < qty {
		qty = b.Amount
	}
	b.Amount -= qty
	other.Amount += qty
}
