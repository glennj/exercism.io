(module
  (memory (export "mem") 1)
 
  (func (export "reverseString") (param $offset i32) (param $length i32) (result i32 i32)
    (local $i i32) (local $j i32)
    (local.set $i (local.get $offset))
    (local.set $j (i32.add (local.get $offset) (i32.sub (local.get $length) (i32.const 1))))
    (block $break
      (loop $while
        (if (i32.ge_u (local.get $i) (local.get $j)) (br $break))
        (call $swap (local.get $i) (local.get $j))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (local.set $j (i32.sub (local.get $j) (i32.const 1)))
        (br $while)))
    (return (local.get $offset) (local.get $length)))

  (func $swap (param i32) (param i32)
    (local $temp i32)
    (local.set $temp (i32.load8_u (local.get 0)))
    (i32.store8 (local.get 0) (i32.load8_u (local.get 1)))
    (i32.store8 (local.get 1) (local.get $temp))))
