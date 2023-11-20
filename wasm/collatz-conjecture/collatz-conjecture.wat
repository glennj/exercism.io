(module
  (func $nextcollatz (param $n i32) (result i32)
    (local.get $n) (i32.const 3) (i32.mul) (i32.const 1) (i32.add)  ;; if odd
    (local.get $n) (i32.const 1) (i32.shr_u)                        ;; if even
    (local.get $n) (i32.const 1) (i32.and) ;; odd or even?
    (select))

  (func $collatz (param $n i32) (param $steps i32) (result i32)
    (local.get $n) (i32.const 1)
    (if (i32.eq) (then (return (local.get $steps))))

    (call $nextcollatz (local.get $n))
    (local.get $steps) (i32.const 1) (i32.add)
    (call $collatz))


  (func (export "steps") (param $number i32) (result i32)
    (local.get $number) (i32.const 0)
    (if (i32.le_s) (then (return (i32.const -1))))

    (local.get $number)
    (i32.const 0)
    (call $collatz))
)
