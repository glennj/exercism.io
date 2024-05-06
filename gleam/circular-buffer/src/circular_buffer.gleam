import gleam/queue.{type Queue}

pub opaque type CircularBuffer(t) {
  CircularBuffer(queue: Queue(t), capacity: Int, count: Int)
}

pub fn new(capacity: Int) -> CircularBuffer(t) {
  CircularBuffer(queue.new(), capacity, 0)
}

pub fn read(buffer: CircularBuffer(t)) -> Result(#(t, CircularBuffer(t)), Nil) {
  case queue.pop_back(buffer.queue) {
    Error(_) -> Error(Nil)
    Ok(#(e, q)) ->
      Ok(#(e, CircularBuffer(q, buffer.capacity, buffer.count - 1)))
  }
}

pub fn write(
  buffer: CircularBuffer(t),
  item: t,
) -> Result(CircularBuffer(t), Nil) {
  case is_full(buffer) {
    True -> Error(Nil)
    False -> {
      Ok(CircularBuffer(
        queue.push_front(buffer.queue, item),
        buffer.capacity,
        buffer.count + 1,
      ))
    }
  }
}

pub fn overwrite(buffer: CircularBuffer(t), item: t) -> CircularBuffer(t) {
  let buff = case is_full(buffer) {
    True -> {
      let assert Ok(#(_, b)) = read(buffer)
      b
    }
    False -> buffer
  }
  let assert Ok(buff) = write(buff, item)
  buff
}

pub fn clear(buffer: CircularBuffer(t)) -> CircularBuffer(t) {
  new(buffer.capacity)
}

fn is_full(buffer: CircularBuffer(t)) {
  buffer.count == buffer.capacity
}
