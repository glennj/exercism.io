(module
  (memory (export "mem") 1)

  (data (i32.const 0) "One for ")       ;; prefix, length 8
  (data (i32.const 8) "you")            ;; default, length 3
  (data (i32.const 11) ", one for me.") ;; suffix, length 13
  ;; we can start writing the output at offset 24
  (global $start i32 (i32.const 24))


  (func $copy_and_inc (param $dest i32) (param $src i32) (param $len i32) (result i32)
    (memory.copy (local.get $dest) (local.get $src) (local.get $len))
    (i32.add (local.get $dest) (local.get $len)))


  (func (export "twoFer") (param $offset i32) (param $length i32) (result i32 i32)
    (local $p i32) (local $start i32)

    (if (i32.eqz (local.get $length))
      (then (return (i32.const 0) (i32.const 24))))

    (local.set $p (global.get $start))
    (local.set $p (call $copy_and_inc (local.get $p) (i32.const 0) (i32.const 8)))
    (local.set $p (call $copy_and_inc (local.get $p) (local.get $offset) (local.get $length)))
    (local.set $p (call $copy_and_inc (local.get $p) (i32.const 11) (i32.const 13)))

    (return
      (global.get $start)
      (i32.sub (local.get $p) (global.get $start)))))
