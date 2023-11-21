(module
  (memory (export "mem") 1)

  (global $pangramValue i32 (i32.const 0x3ffffff))  ;; (1 << 26) - 1

  (func $is-upper (param i32) (result i32)
    (i32.and (i32.le_u (i32.const 65) (local.get 0))
             (i32.le_u (local.get 0) (i32.const 90))))

  (func $is-lower (param i32) (result i32)
    (i32.and (i32.le_u (i32.const 97) (local.get 0))
             (i32.le_u (local.get 0) (i32.const 122))))

  (func $is-alpha (param i32) (result i32)
    (i32.or (call $is-upper (local.get 0))
            (call $is-lower (local.get 0))))

  (func $char-index (param i32) (result i32)
    (if (call $is-upper (local.get 0))
      (then (return (i32.sub (local.get 0) (i32.const 65)))))
    (return (i32.sub (local.get 0) (i32.const 97))))

  ;;
  ;; Determine if a string is a pangram.
  ;;
  ;; @param {i32} offset - offset of string in linear memory
  ;; @param {i32} length - length of string in linear memory
  ;;
  ;; @returns {i32} 1 if pangram, 0 otherwise
  ;;
  (func (export "isPangram") (param $offset i32) (param $length i32) (result i32)
    (local $bitfield i32) (local $mask i32) (local $i i32) (local $c i32)

    (loop $loop (block $break
      (br_if $break (i32.eq (local.get $i) (local.get $length)))
      (local.tee $c (i32.load8_u (i32.add (local.get $offset) (local.get $i))))
      (if (call $is-alpha) (then
        (local.set $mask (i32.shl (i32.const 1) (call $char-index (local.get $c))))
        (local.set $bitfield (i32.or (local.get $bitfield) (local.get $mask))))
        (br_if $break (i32.eq (local.get $bitfield) (global.get $pangramValue))))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $loop)))
    (i32.eq (local.get $bitfield) (global.get $pangramValue))))
