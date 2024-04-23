pub fn secret_add(secret: Int) -> fn(Int) -> Int {
  fn(n: Int) { n + secret}
}

pub fn secret_subtract(secret: Int) -> fn(Int) -> Int {
  secret_add(-secret)
}

pub fn secret_multiply(secret: Int) -> fn(Int) -> Int {
  fn(n: Int) { n * secret}
}

pub fn secret_divide(secret: Int) -> fn(Int) -> Int {
  fn(n: Int) {n / secret}
}

pub fn secret_combine(f1: fn(Int) -> Int, f2: fn(Int) -> Int) -> fn(Int) -> Int {
  fn(n: Int) {n |> f1 |> f2}
}
