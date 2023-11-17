(module
  (func $counter (param $n i32) (param $count i32) (result i32)
    (i32.eq (local.get $n) (i32.const 0))
    (if (then (return (local.get $count))))

    (call $counter
      (i32.shr_u (local.get $n) (i32.const 1))
      (i32.add (local.get $count) (i32.and (local.get $n) (i32.const 1)))))


  (func (export "eggCount") (param $number i32) (result i32)
    (call $counter (local.get $number) (i32.const 0))))
