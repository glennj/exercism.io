package circular

/* This implementation uses a variable-size slice of bytes,
 * and remembers the maximum capacity of the buffer.
 * All reads are from index 0 of the slice.
 * All writes are appended to the slice.
 *
 * I'm surprised this is 4x faster
 * than the read/write index implementation.
 */

import "errors"

var (
	ErrEmpty = errors.New("buffer is empty")
	ErrFull  = errors.New("buffer is full")
)

type Buffer struct {
	bytes    []byte
	capacity int
}

func NewBuffer(size int) *Buffer {
	b := Buffer{capacity: size}
	b.Reset()
	return &b
}

func (b *Buffer) Reset() {
	b.bytes = make([]byte, 0, b.capacity)
}

func (b *Buffer) isFull() bool {
	return len(b.bytes) == b.capacity
}

func (b *Buffer) isEmpty() bool {
	return len(b.bytes) == 0
}

func (b *Buffer) ReadByte() (byte, error) {
	if b.isEmpty() {
		return 0, ErrEmpty
	}
	value := b.bytes[0]
	b.bytes = b.bytes[1:]
	return value, nil
}

func (b *Buffer) WriteByte(c byte) error {
	if b.isFull() {
		return ErrFull
	}
	b.bytes = append(b.bytes, c)
	return nil
}

func (b *Buffer) Overwrite(c byte) {
	if b.isFull() {
		b.ReadByte()
	}
	b.WriteByte(c)
}

/* benchmarks
 * goos: linux
 * goarch: amd64
 * pkg: standard
 * cpu: Intel(R) Core(TM) i7-9750H CPU @ 2.60GHz
 * BenchmarkOverwrite
 * BenchmarkOverwrite-2   	336430161	         3.449 ns/op	97540219075.25 MB/s	       1 B/op	       0 allocs/op
 * BenchmarkWriteRead
 * BenchmarkWriteRead-2   	265769714	         4.663 ns/op	56989465683.38 MB/s	       1 B/op	       0 allocs/op
 */

// Implement a circular buffer of bytes supporting both overflow-checked writes
// and unconditional, possibly overwriting, writes.
//
// We chose the provided API so that Buffer implements io.ByteReader
// and io.ByteWriter and can be used (size permitting) as a drop in
// replacement for anything using that interface.
