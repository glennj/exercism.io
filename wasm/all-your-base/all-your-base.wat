(module
  (memory (export "mem") 1)

  ;; Status codes returned as res[2]
  (global $ok i32 (i32.const 0))
  (global $inputHasWrongFormat i32 (i32.const -1))
  (global $wrongInputBase i32 (i32.const -2))
  (global $wrongOutputBase i32 (i32.const -3))


  (func $convert-to-decimal (param $p i32) (param $len i32) (param $base i32) (result i32)
    (local $dec i32) (local $i i32) (local $digit i32)
    (local $a i32) (local $b i32)

    (if (i32.and (i32.eqz (i32.load (local.get $p)))
                 (i32.gt_u (local.get $len) (i32.const 1)))
      ;; leading zero
      (then (return (global.get $inputHasWrongFormat))))

    (loop $loop (result i32)
      (local.set $digit (i32.load (local.get $p)))

      (if (i32.or (i32.lt_s (local.get $digit) (i32.const 0))
                  (i32.ge_s (local.get $digit) (local.get $base)))
        ;; invalid digit
        (then (return (global.get $inputHasWrongFormat))))

      (local.set $dec (i32.add (i32.mul (local.get $dec) (local.get $base))
                               (local.get $digit)))

      (local.set $p (i32.add (local.get $p) (i32.const 4)))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (if (i32.eq (local.get $i) (local.get $len))
        (then (return (local.get $dec))))
      (br $loop)))


  (func $reverse-i32-arr (param $p i32) (param $len i32)
    (local $i i32) (local $j i32) (local $tmp i32)
    (local.set $i (local.get $p))
    (local.set $j (i32.add (i32.mul (i32.sub (local.get $len) (i32.const 1))
                                    (i32.const 4))
                           (local.get $p)))
    (loop $loop (block $break
      (br_if $break (i32.ge_u (local.get $i) (local.get $j)))
      (local.set $tmp (i32.load (local.get $i)))
      (i32.store (local.get $i) (i32.load (local.get $j)))
      (i32.store (local.get $j) (local.get $tmp))
      (local.set $i (i32.add (local.get $i) (i32.const 4)))
      (local.set $j (i32.sub (local.get $j) (i32.const 4)))
      (br $loop))))


  (func $convert-from-decimal (param $dec i32) (param $base i32) (param $location i32) (result i32)
    (local $len i32) (local $digit i32)
    (local $p i32)
    (local.set $p (local.get $location))

    (loop $loop
      (local.set $digit (i32.rem_u (local.get $dec) (local.get $base)))
      (i32.store (local.get $p) (local.get $digit))
      (local.set $dec (i32.div_u (local.get $dec) (local.get $base)))
      (local.set $p (i32.add (local.get $p) (i32.const 4)))
      (local.set $len (i32.add (local.get $len) (i32.const 1)))
      (br_if $loop (i32.gt_u (local.get $dec) (i32.const 0))))
    (call $reverse-i32-arr (local.get $location) (local.get $len))
    (return (local.get $len)))


  (func $err (param i32) (result i32 i32 i32)
    (i32.const 0) (i32.const 0) (local.get 0))


  ;;
  ;; Convert an array of digits in inputBase to an array of digits in outputBase
  ;; @param {i32} arrOffset - base offset of input u32[] array
  ;; @param {i32} arrLength - length of the input u32[] array in elements
  ;; @param {i32} inputBase - base of the input array
  ;; @param {i32} outputBase - base of the output array
  ;; @return {i32} - base offset of the output u32[] array
  ;; @return {i32} - length of the output u32[] array in elements
  ;; @return {i32} - status code (0, -1, -2, -3)
  ;;
  (func (export "convert") (param $arrOffset i32) (param $arrLength i32) (param $inputBase i32) (param $outputBase i32) (result i32 i32 i32)
    (local $decimalValue i32)
    (local $returnOffset i32)
    (local $returnLength i32)

    (if (i32.lt_s (local.get $inputBase) (i32.const 2))
      (then (return (call $err (global.get $wrongInputBase)))))

    (if (i32.lt_s (local.get $outputBase) (i32.const 2))
      (then (return (call $err (global.get $wrongOutputBase)))))

    (if (i32.eqz (local.get $arrLength))
      (then (return (call $err (global.get $inputHasWrongFormat)))))

    (local.set $decimalValue
      (call $convert-to-decimal (local.get $arrOffset)
                                (local.get $arrLength)
                                (local.get $inputBase)))

    (if (i32.lt_s (local.get $decimalValue) (i32.const 0))
      (then (return (call $err (local.get $decimalValue)))))

    (local.set $returnOffset (i32.const 0))
    (local.set $returnLength
      (call $convert-from-decimal (local.get $decimalValue)
                                  (local.get $outputBase)
                                  (local.get $returnOffset)))
    (return (local.get $returnOffset) (local.get $returnLength) (global.get $ok))))
