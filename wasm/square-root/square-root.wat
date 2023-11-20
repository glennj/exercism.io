(module
  ;; Using the Binary numeral system implementation from
  ;; https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_(base_2)
  (func (export "squareRoot") (param $n i32) (result i32)
    (local $b i32)
    (local $x i32)

    ;; find b, the largest power of 4 <= n
    (local.set $b (i32.const 1))
    (loop $loop
      (local.set $b (i32.shl (local.get $b) (i32.const 2)))
      (br_if $loop (i32.le_u (local.get $b) (local.get $n))))
    (local.set $b (i32.shr_u (local.get $b) (i32.const 2)))

    ;; calculate x, the square root of n
    (loop $loop (result i32)
      (if (i32.eqz (local.get $b))
        (then (return (local.get $x))))
      (if (i32.ge_u (local.get $n) (i32.add (local.get $x) (local.get $b)))
        (then
          (local.set $n (i32.sub (local.get $n) (i32.add (local.get $x) (local.get $b))))
          (local.set $x (i32.add (i32.shr_u (local.get $x) (i32.const 1)) (local.get $b))))
        (else
          (local.set $x (i32.shr_u (local.get $x) (i32.const 1)))))
      (local.set $b (i32.shr_u (local.get $b) (i32.const 2)))
      (br $loop))))
