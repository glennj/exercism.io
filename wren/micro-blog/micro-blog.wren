import "./math" for Math

class MicroBlog {
  static truncate(s) { truncate(s, 5) }
  static truncate(s, len) { s.take(Math.min(s.count, len)).join() }
}
