import java.util.Collections;
import java.util.NoSuchElementException;
import java.util.Queue;
import java.util.Vector;
import java.util.concurrent.ArrayBlockingQueue;

interface ICircularBuffer<T> {
    void clear();
    T read() throws BufferIOException;
    void write(T element) throws BufferIOException;
    void overwrite(T element);
}

class CircularBuffer<T> {
    private ICircularBuffer<T> implementation;

    CircularBuffer(int size) {
        // implementation = new StackBasedBuffer<>(size);
        implementation = new PointerBasedBuffer<>(size);
    }

    void clear() { implementation.clear(); }
    T read() throws BufferIOException { return implementation.read(); }
    void write(T element) throws BufferIOException { implementation.write(element); }
    void overwrite(T element) { implementation.overwrite(element); }

    /* ********************************************************************** */
    /**
     * An implementation of a CircularBuffer more closely aligned to the
     * description of the exercise: a buffer of a predefined length that maintains
     * a read pointer and a write pointer.
     *
     * @param <T>
     */
    private static class PointerBasedBuffer<T> implements ICircularBuffer<T> {

        private static class Cursor {
            private int read = 0;
            private int write = 0;
            private int size;
            Cursor(int size) { this.size = size; }
            int getRead() { return read; }
            int getWrite() { return write; }
            void incrRead() { read = (read + 1) % size; }
            void incrWrite() { write = (write + 1) % size; }
        }

        private Vector<T> buffer;
        private int size;
        private Cursor cursor;

        PointerBasedBuffer(int size) {
            this.size = size;
            clear();
        }

        public void clear() {
            buffer = new Vector<>(Collections.nCopies(size, null));
            cursor = new Cursor(size);
        }

        private boolean isEmpty() {
            return buffer.get(cursor.getRead()) == null;
        }

        public T read() throws BufferIOException {
            if (isEmpty())
                throw new BufferIOException("Tried to read from empty buffer");

            T element = buffer.get(cursor.getRead());
            buffer.set(cursor.getRead(), null);
            cursor.incrRead();
            return element;
        }

        private boolean isFull() {
            return cursor.getRead() == cursor.getWrite() && buffer.get(cursor.getRead()) != null;
        }

        public void write(T element) throws BufferIOException {
            if (isFull())
                throw new BufferIOException("Tried to write to full buffer");

            buffer.set(cursor.getWrite(), element);
            cursor.incrWrite();
        }

        public void overwrite(T element) {
            try {
                if (isFull()) read();
                write(element);
            }
            catch (BufferIOException e) {
                // won't happen: we know the buffer is:
                // a) not empty when we read, and
                // b) not full when we write.
            }
        }
    }

    /* ********************************************************************** */
    /**
     * A circular buffer implementation using a Queue as the backend storage.
     * The read pointer is always at the head of the queue.
     * The write pointer is always at the tail of the queue.
     *
     * @param <T>
     */
    private static class StackBasedBuffer<T> implements ICircularBuffer<T> {
        private Queue<T> buffer;
        private int size;

        StackBasedBuffer(int size) {
            this.size = size;
            clear();
        }

        public void clear() {
            // ArrayBlockingQueue will throw exceptions attempting to
            // read from empty deque or write to full deque
            buffer = new ArrayBlockingQueue<>(size);
        }

        public T read() throws BufferIOException {
            try {
                return buffer.remove();
            } catch (NoSuchElementException e) {
                throw new BufferIOException("Tried to read from empty buffer");
            }
        }

        public void write(T element) throws BufferIOException {
            try {
                buffer.add(element);
            } catch (IllegalStateException e) {
                throw new BufferIOException("Tried to write to full buffer");
            }
        }

        public void overwrite(T element) {
            try {
                if (isFull()) read();
                write(element);
            } catch (BufferIOException e) {
                // won't happen: we know the buffer is:
                // a) not empty when we read, and
                // b) not full when we write.
            }
        }

        private boolean isFull() {
            return buffer.size() == size;
        }
    }
}
