module circular;

import std.exception : enforce;

class Buffer(E) {

  private:
    immutable size_t capacity;
    E[] buffer;
    size_t readPtr, writePtr;
    size_t count;

    void incr(ref size_t ptr) {
        ptr = (ptr + 1) % capacity;
    }

  public:
    this(size_t capacity) {
        this.capacity = capacity;
        this.buffer = new E[capacity];
        clear();
    }

    void clear() {
        readPtr = writePtr = 0;
        count = 0;
    }

    bool isEmpty() => count == 0;
    bool isFull() => count == capacity;

    E pop() {
        enforce(!isEmpty(), "empty buffer");
        auto element = buffer[readPtr];
        incr(readPtr);
        count--;
        return element;
    }

    void push(E element) {
        enforce(!isFull(), "full buffer");
        buffer[writePtr] = element;
        incr(writePtr);
        count++;
    }

    void forcePush(E element) {
        if (isFull()) pop();
        push(element);
    }
}

unittest
{
    import std.exception : assertThrown;

    immutable int allTestsEnabled = 1;

    // Reading empty buffer should fail"
    {
        auto myBuffer = new Buffer!(int)(1UL);
        assertThrown(myBuffer.pop(), "Empty buffer should throw exception if popped!");
    }

    static if (allTestsEnabled)
    {

        // Can read an item just written
        {
            auto myBuffer = new Buffer!(char)(1);
            myBuffer.push('1');
            assert(myBuffer.pop() == '1');
        }

        // Each item may only be read once"
        {
            auto myBuffer = new Buffer!(char)(1);
            myBuffer.push('1');
            assert(myBuffer.pop() == '1');
            assertThrown(myBuffer.pop(), "Empty buffer should throw exception if popped!");
        }

        // Items are read in the order they are written
        {
            auto myBuffer = new Buffer!(char)(2);
            myBuffer.push('1');
            myBuffer.push('2');
            assert(myBuffer.pop() == '1');
            assert(myBuffer.pop() == '2');
        }

        // Full buffer can't be written to
        {
            auto myBuffer = new Buffer!(char)(1);
            myBuffer.push('1');
            assertThrown(myBuffer.push('2'),
                    "Full buffer should throw exception if new element pushed!");
        }

        // A read frees up capacity for another write
        {
            auto myBuffer = new Buffer!(char)(1);
            myBuffer.push('1');
            assert(myBuffer.pop() == '1');
            myBuffer.push('2');
            assert(myBuffer.pop() == '2');
        }

        // Read position is maintained even across multiple writes
        {
            auto myBuffer = new Buffer!(char)(3);
            myBuffer.push('1');
            myBuffer.push('2');
            assert(myBuffer.pop() == '1');
            myBuffer.push('3');
            assert(myBuffer.pop() == '2');
            assert(myBuffer.pop() == '3');
        }

        // Items cleared out of buffer can't be read
        {
            auto myBuffer = new Buffer!(char)(1);
            myBuffer.push('1');
            myBuffer.clear();
            assertThrown(myBuffer.pop(), "Empty buffer should throw exception if popped!");
        }

        // Clear frees up capacity for another write
        {
            auto myBuffer = new Buffer!(char)(1);
            myBuffer.push('1');
            myBuffer.clear();
            myBuffer.push('2');
            assert(myBuffer.pop() == '2');
        }

        // Clear does nothing on empty buffer
        {
            auto myBuffer = new Buffer!(char)(1);
            myBuffer.clear();
            myBuffer.push('1');
            assert(myBuffer.pop() == '1');
        }

        // Overwrite acts like write on non-full buffer
        {
            auto myBuffer = new Buffer!(char)(2);
            myBuffer.push('1');
            myBuffer.forcePush('2');
            assert(myBuffer.pop() == '1');
            assert(myBuffer.pop() == '2');
        }

        // Overwrite replaces the oldest item on full buffer
        {
            auto myBuffer = new Buffer!(char)(2);
            myBuffer.push('1');
            myBuffer.push('2');
            myBuffer.forcePush('3');
            assert(myBuffer.pop() == '2');
            assert(myBuffer.pop() == '3');
        }

        // Overwrite replaces the oldest item remaining in buffer following a read
        {
            auto myBuffer = new Buffer!(char)(3);
            myBuffer.push('1');
            myBuffer.push('2');
            myBuffer.push('3');
            assert(myBuffer.pop() == '1');
            myBuffer.push('4');
            myBuffer.forcePush('5');
            assert(myBuffer.pop() == '3');
            assert(myBuffer.pop() == '4');
            assert(myBuffer.pop() == '5');
        }

        // Initial clear does not affect wrapping around
        {
            auto myBuffer = new Buffer!(char)(2);
            myBuffer.clear();
            myBuffer.push('1');
            myBuffer.push('2');
            myBuffer.forcePush('3');
            myBuffer.forcePush('4');
            assert(myBuffer.pop() == '3');
            assert(myBuffer.pop() == '4');
            assertThrown(myBuffer.pop(), "Empty buffer should throw exception if popped!");
        }

    }

}
