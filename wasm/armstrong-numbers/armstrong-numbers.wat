(module
  (func $divmod (param $num i32) (param $den i32) (result i32 i32)
    (i32.div_u (local.get $num) (local.get $den))
    (i32.rem_u (local.get $num) (local.get $den)))

  (func $numlen (param $num i32) (result i32)
    (local $len i32)
    (if (i32.eqz (local.get $num))
      (then (return (i32.const 1))))
    (loop $loop (result i32)
      (if (i32.eqz (local.get $num))
        (then (return (local.get $len))))
      (local.set $len (i32.add (local.get $len) (i32.const 1)))
      (local.set $num (i32.div_u (local.get $num) (i32.const 10)))
      (br $loop)))

  (func $pow (param $base i32) (param $exp i32) (result i32)
    (local $result i32)
    (local.set $result (i32.const 1))
    (loop $loop (result i32)
      (if (i32.eqz (local.get $exp))
        (then (return (local.get $result))))
      (if (i32.and (local.get $exp) (i32.const 1))
        (then (local.set $result (i32.mul (local.get $result) (local.get $base)))))
      (local.set $exp (i32.shr_u (local.get $exp) (i32.const 1)))
      (local.set $base (i32.mul (local.get $base) (local.get $base)))
      (br $loop)))

  (func $armstrong-sum (param $n i32) (result i32)
    (local $len i32)
    (local $result i32)
    (local $d i32)
    (local.set $len (call $numlen (local.get $n)))
    (local.set $result (i32.const 0))
    (loop $loop (result i32)
      (if (i32.eqz (local.get $n))
        (then (return (local.get $result))))
      (call $divmod (local.get $n) (i32.const 10))
      (local.set $d)
      (local.set $n)
      (local.set $result (i32.add (local.get $result)
                                  (call $pow (local.get $d) (local.get $len))))
      (br $loop)))

  ;; returns 1 if armstrong number, 0 otherwise
  (func (export "isArmstrongNumber") (param $candidate i32) (result i32)
    (i32.eq (local.get $candidate)
            (call $armstrong-sum (local.get $candidate)))))