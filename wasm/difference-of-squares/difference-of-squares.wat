(module
  (func $ident  (param i32) (result i32) (local.get 0))
  (func $square (param i32) (result i32) (i32.mul (local.get 0) (local.get 0)))

  (table 2 funcref)
  (elem (i32.const 0) $ident $square)
  (type $f (func (param i32) (result i32)))

  ;; iterate from 1 to max,
  ;; accumulating the sum of f1(i),
  ;; returning f2(sum)
  (func $do-loop (param $max i32) (param $f1 i32) (param $f2 i32) (result i32)
    (local $sum i32)
    (local $i i32)
    (loop $loop
      (local.tee $i (i32.add (local.get $i) (i32.const 1)))
      (call_indirect (type $f) (local.get $f1))
      (local.set $sum (i32.add (local.get $sum)))
      (br_if $loop (i32.lt_s (local.get $i) (local.get $max))))
    (local.get $sum)
    (call_indirect (type $f) (local.get $f2)))
     

  (func $squareOfSum (export "squareOfSum") (param $max i32) (result i32)
    (call $do-loop (local.get $max) (i32.const 0) (i32.const 1)))

  (func $sumOfSquares (export "sumOfSquares") (param $max i32) (result i32)
    (call $do-loop (local.get $max) (i32.const 1) (i32.const 0)))

  (func (export "difference") (param $max i32) (result i32)
    (i32.sub (call $squareOfSum (local.get $max))
             (call $sumOfSquares (local.get $max)))))
