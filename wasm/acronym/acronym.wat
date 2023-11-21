(module
  (memory (export "mem") 1)
  (global $resultOffset i32 (i32.const 0))


  (func $is-upper (param i32) (result i32)
    (i32.and (i32.le_u (i32.const 65) (local.get 0))
             (i32.le_u (local.get 0) (i32.const 90))))

  (func $is-lower (param i32) (result i32)
    (i32.and (i32.le_u (i32.const 97) (local.get 0))
             (i32.le_u (local.get 0) (i32.const 122))))

  (func $is-alpha (param i32) (result i32)
    (i32.or (call $is-upper (local.get 0))
            (call $is-lower (local.get 0))))

  (func $is-alpha-or-apostrophe (param i32) (result i32)
    (i32.or (call $is-alpha (local.get 0))
            (i32.eq (local.get 0) (i32.const 39))))

  (func $to-upper (param i32) (result i32)
    (if (call $is-lower (local.get 0))
      (then (local.set 0 (i32.sub (local.get 0) (i32.const 32)))))
    (local.get 0))

  (func $inc (param i32) (result i32) (i32.add (local.get 0) (i32.const 1)))
  (func $not (param i32) (result i32) (i32.xor (local.get 0) (i32.const 1)))


  (func (export "parse") (param $offset i32) (param $length i32) (result i32 i32)
    (local $i i32) (local $c i32) (local $len i32) (local $in-word i32)

    (loop $loop
      (block $break
        (br_if $break (i32.eq (local.get $i) (local.get $length)))
        (local.set $c (i32.load8_u (i32.add (local.get $offset) (local.get $i))))

        (if (local.get $in-word)

          ;; seeking a non-word-char
          (then
            (if (call $not (call $is-alpha-or-apostrophe (local.get $c)))
              (then
                (local.set $in-word (i32.const 0)))))

          ;; seeking first char of a word
          (else
            (if (call $is-alpha (local.get $c))
              (then
                (i32.store8 (i32.add (global.get $resultOffset) (local.get $len))
                            (call $to-upper (local.get $c)))
                (local.set $len (call $inc (local.get $len)))
                (local.set $in-word (i32.const 1))))))

        (local.set $i (call $inc (local.get $i)))
        (br $loop)))

    (return (global.get $resultOffset) (local.get $len))))
