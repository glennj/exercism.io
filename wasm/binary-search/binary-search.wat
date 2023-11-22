(module
  (memory (export "mem") 1)
 
  ;;
  ;; Find the first occurrence of the needle in the haystack
  ;;
  ;; @param {i32} base - the base address of the haystack
  ;; @param {i32} nelems - the number of elements in the haystack
  ;; @param {i32} needle - the value to search for
  ;;
  ;; @return {i32} the index of the first occurrence of the needle in the haystack
  ;;               or -1 if the needle is not found.
  ;;
  (func (export "find") (param $base i32) (param $nelems i32) (param $needle i32) (result i32)
    (local $i i32) (local $j i32) (local $mid i32) (local $val i32)
    (local.set $i (i32.const 0))
    (local.set $j (i32.sub (local.get $nelems) (i32.const 1)))

    (loop $loop (result i32)
      (if (i32.gt_s (local.get $i) (local.get $j))
        (then (return (i32.const -1))))

      (local.set $mid (i32.shr_u (i32.add (local.get $i) (local.get $j)) (i32.const 1)))
      (local.set $val (i32.load (i32.add (local.get $base)
                                         (i32.mul (local.get $mid) (i32.const 4)))))

      (if (i32.eq (local.get $needle) (local.get $val))
        (then (return (local.get $mid))))

      (if (i32.gt_u (local.get $needle) (local.get $val))
        (then (local.set $i (i32.add (local.get $mid) (i32.const 1))))
        (else (local.set $j (i32.sub (local.get $mid) (i32.const 1)))))

      (br $loop))))
