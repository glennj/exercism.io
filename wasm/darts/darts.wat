(module
  (func $hypot (param $x f32) (param $y f32) (result f32)
    (f32.mul (local.get $x) (local.get $x))
    (f32.mul (local.get $y) (local.get $y))
    (f32.add)
    (f32.sqrt))

  (func (export "score") (param $x f32) (param $y f32) (result i32)
    (local $dist f32)

    (call $hypot (local.get $x) (local.get $y))
    (local.set $dist)

    (f32.le (local.get $dist) (f32.const 1.0))
    (if (then (i32.const 10) (return)))

    (f32.le (local.get $dist) (f32.const 5.0))
    (if (then (i32.const 5) (return)))

    (f32.le (local.get $dist) (f32.const 10.0))
    (if (then (i32.const 1) (return)))

    (i32.const 0) (return)))
