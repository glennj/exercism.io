(module
  (func $divisbleby (param $num i32) (param $den i32) (result i32)
    (i32.eqz (i32.rem_u (local.get $num) (local.get $den))))

  (func $not (param $bool i32) (result i32)
    (i32.xor (i32.const 1) (local.get $bool)))

  ;; Returns 1 if leap year, 0 otherwise
  (func (export "isLeap") (param $year i32) (result i32)
    (i32.and
      (call $divisbleby (local.get $year) (i32.const 4))
      (i32.or
        (call $not (call $divisbleby (local.get $year) (i32.const 100)))
        (call $divisbleby (local.get $year) (i32.const 400)))))
)
