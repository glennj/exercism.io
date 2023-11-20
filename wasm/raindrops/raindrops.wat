(module
  (memory (export "mem") 1)

  (data (i32.const 0) "Pling")
  (data (i32.const 5) "Plang")
  (data (i32.const 10) "Plong")

  (global $char-0 i32 (i32.const 48))   ;; ASCII '0' == 48

  ;; returns true if $div evenly divides $num
  (func $divisible-by (param $num i32) (param $div i32) (result i32)
    (i32.rem_u (local.get $num) (local.get $div))
    (i32.eqz))

  ;; stores the string representation of the number at the given memory location.
  ;; returns the string length.
  ;; does not handle zero or negative numbers.
  (func $itoa (param $num i32) (param $location i32) (result i32)
    (local $len i32)
    (local $p i32)
    (local $d i32)
    (local.set $p (i32.const 512))
    (block $break
      (loop $while
        (if (i32.eqz (local.get $num)) (br $break))
        (local.set $d   (i32.rem_u (local.get $num) (i32.const 10)))
        (local.set $num (i32.div_u (local.get $num) (i32.const 10)))
        (i32.store8 (local.get $p) (i32.add (local.get $d) (global.get $char-0)))
        (local.set $p (i32.sub (local.get $p) (i32.const 1)))
        (local.set $len (i32.add (local.get $len) (i32.const 1)))
        (br $while)))
    (memory.copy (local.get $location)
                (i32.add (local.get $p) (i32.const 1)) (local.get $len))
    (return (local.get $len)))


  (func (export "convert") (param $input i32) (result i32 i32)
    (local $p i32) (local $len i32)
    (local.set $p (i32.const 20))

    (if (call $divisible-by (local.get $input) (i32.const 3))
      (then
        (memory.copy (local.get $p)
                     (i32.const 0) (i32.const 5))
        (local.set $len (i32.add (local.get $len) (i32.const 5)))))

    (if (call $divisible-by (local.get $input) (i32.const 5))
      (then
        (memory.copy (i32.add (local.get $p) (local.get $len))
                     (i32.const 5) (i32.const 5))
        (local.set $len (i32.add (local.get $len) (i32.const 5)))))

    (if (call $divisible-by (local.get $input) (i32.const 7))
      (then
        (memory.copy (i32.add (local.get $p) (local.get $len))
                     (i32.const 10) (i32.const 5))
        (local.set $len (i32.add (local.get $len) (i32.const 5)))))

    (if (i32.eqz (local.get $len))
      (then
        (local.set $len (call $itoa (local.get $input) (local.get $p)))))

    (return (local.get $p) (local.get $len))))
