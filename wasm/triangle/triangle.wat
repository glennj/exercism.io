(module
  (func $triangle-equality (param f32 f32 f32) (result i32)
    (i32.and
      ;; no side is too long
      (i32.and
        (f32.le (local.get 0) (f32.add (local.get 1) (local.get 2)))
        (i32.and
          (f32.le (local.get 1) (f32.add (local.get 0) (local.get 2)))
          (f32.le (local.get 2) (f32.add (local.get 0) (local.get 1)))))
      ;; no side is zero length
      (i32.and
        (f32.gt (local.get 0) (f32.const 0))
        (i32.and
          (f32.gt (local.get 1) (f32.const 0))
          (f32.gt (local.get 2) (f32.const 0))))))

  (func $not (param i32) (result i32) (i32.xor (local.get 0) (i32.const 1)))

  (func $is-equilateral (param f32 f32 f32) (result i32)
    (i32.and
      (f32.eq (local.get 0) (local.get 1))
      (i32.and
        (f32.eq (local.get 0) (local.get 2))
        (f32.eq (local.get 1) (local.get 2)))))

  (func $is-isosceles (param f32 f32 f32) (result i32)
    (i32.or
      (f32.eq (local.get 0) (local.get 1))
      (i32.or
        (f32.eq (local.get 0) (local.get 2))
        (f32.eq (local.get 1) (local.get 2)))))

  ;; Is the triangle equilateral?
  ;;
  ;; @param {f32} length of side a
  ;; @param {f32} length of side b
  ;; @param {f32} length of side c
  ;;
  ;; @returns {i32} 1 if equalateral, 0 otherwise
  (func (export "isEquilateral") (param f32 f32 f32) (result i32)
    (i32.and
      (call $triangle-equality (local.get 0) (local.get 1) (local.get 2))
      (call $is-equilateral (local.get 0) (local.get 1) (local.get 2))))

  ;; Is the triangle isosceles?
  ;;
  ;; @param {f32} length of side a
  ;; @param {f32} length of side b
  ;; @param {f32} length of side c
  ;;
  ;; @returns {i32} 1 if isosceles, 0 otherwise
  (func (export "isIsosceles") (param f32 f32 f32) (result i32)
    (i32.and
      (call $triangle-equality (local.get 0) (local.get 1) (local.get 2))
      (call $is-isosceles (local.get 0) (local.get 1) (local.get 2))))

  ;; Is the triangle scalene?
  ;;
  ;; @param {f32} length of side a
  ;; @param {f32} length of side b
  ;; @param {f32} length of side c
  ;;
  ;; @returns {i32} 1 if scalene, 0 otherwise
  (func (export "isScalene") (param f32 f32 f32) (result i32)
    (i32.and
      (call $triangle-equality (local.get 0) (local.get 1) (local.get 2))
      (call $not (call $is-isosceles (local.get 0) (local.get 1) (local.get 2))))))
