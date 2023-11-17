(module
  (func $nextcollatz (param $n i32) (result i32)
    ;; push 2 values onto stack
    (i32.add (i32.mul (local.get $n) (i32.const 3)) (i32.const 1))
    (i32.shr_u (local.get $n) (i32.const 1))

    (i32.and (local.get $n) (i32.const 1)) ;; odd or even?
    (select))

  (func $collatz (param $n i32) (param $steps i32) (result i32)
    (i32.eq (local.get $n) (i32.const 1))
    (if (then (return (local.get $steps))))

    (call $nextcollatz (local.get $n))
    (i32.add (i32.const 1) (local.get $steps))
    (call $collatz))


  (func (export "steps") (param $number i32) (result i32)
    (i32.le_s (local.get $number) (i32.const 0))
    (if (then (return (i32.const -1))))

    (local.get $number)
    (i32.const 0)
    (call $collatz))
)
