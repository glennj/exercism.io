(module
  (global $state   (mut i32) (i32.const 0))
  (global $balance (mut i32) (i32.const 0))

  (global $ok      i32 (i32.const  0))
  (global $fail    i32 (i32.const -1))
  (global $invalid i32 (i32.const -2))

  (func $is-closed (result i32) (i32.eqz (global.get $state)))
  (func $is-open   (result i32) (i32.eq (global.get $state) (i32.const 1)))

  (func (export "open") (result i32)
    (if (call $is-open) (return (global.get $fail)))
    (global.set $state (i32.const 1))
    (global.get $ok))

  (func (export "close") (result i32)
    (if (call $is-closed) (return (global.get $fail)))
    (global.set $state (i32.const 0))
    (global.set $balance (i32.const 0))
    (global.get $ok))

  (func (export "deposit") (param $amount i32) (result i32)
    (if (call $is-closed) (return (global.get $fail)))
    (if (i32.lt_s (local.get $amount) (i32.const 0)) (return (global.get $invalid)))
    (global.set $balance (i32.add (global.get $balance) (local.get $amount)))
    (global.get $ok))

  (func (export "withdraw") (param $amount i32) (result i32)
    (if (call $is-closed) (return (global.get $fail)))
    (if (i32.lt_s (global.get $balance) (local.get $amount)) (return (global.get $invalid)))
    (if (i32.lt_s (local.get $amount) (i32.const 0)) (return (global.get $invalid)))
    (global.set $balance (i32.sub (global.get $balance) (local.get $amount)))
    (global.get $ok))

  (func (export "balance") (result i32)
    (if (call $is-closed) (return (global.get $fail)))
    (global.get $balance)))
