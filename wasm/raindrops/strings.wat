(module
  (import "console" "log_mem_as_utf8" (func $log_mem_as_utf8 (param $byteOffset i32) (param $length i32)))
  (import "console" "log_i32_u" (func $log_i32_u (param i32)))

  (memory (export "mem") 1)

  (func (export "itoa") (param $num i32) (param $location i32) (result i32)
    (local $len i32)
    (local $p i32)
    (local $d i32)
    (local.set $p (i32.const 512))
    (block $break
      (loop $while
        (if (i32.eqz (local.get $num)) (br $break))
        (local.set $d   (i32.rem_u (local.get $num) (i32.const 10)))
        (local.set $num (i32.div_u (local.get $num) (i32.const 10)))
        (i32.store8 (local.get $p) (i32.add (local.get $d) (i32.const 48))) ;; '0' => ascii 48
        (local.set $p (i32.sub (local.get $p) (i32.const 1)))
        (local.set $len (i32.add (local.get $len) (i32.const 1)))
        (br $while)))
    (memory.copy (local.get $location)
                 (i32.add (local.get $p) (i32.const 1)) (local.get $len))
    (call $log_i32_u  (local.get $len))
    (call $log_i32_u  (local.get $len))
    (call $log_mem_as_utf8 (local.get $location) (local.get $len))
    (return (local.get $len))))
