(module
  (memory (export "mem") 1)

  ;; @param {i32} 0 - The character to translate
  ;; @param {i32} 1 - The character to match
  ;; @param {i32} 2 - The replacement character if matched
  ;; @param {i32} 3 - The offset at which to write
  ;; @returns {i32} - Boolean, is match?
  (func $convert (param i32 i32 i32 i32) (result i32)
    (if (i32.eq (local.get 0) (local.get 1))
      (then (i32.store8 (local.get 3) (local.get 2))
            (return (i32.const 1))))
    (return (i32.const 0)))

  ;;
  ;; Convert a string of DNA to RNA
  ;;
  ;; @param {i32} offset - The offset of the DNA string in linear memory
  ;; @param {i32} length - The length of the DNA string in linear memory
  ;;
  ;; @returns {(i32,i32)} - The offset and length of the RNA string in linear memory
  ;;
  (func (export "toRna") (param $offset i32) (param $length i32) (result i32 i32)
    (local $nuc i32) (local $i i32) (local $p i32)

    (loop $loop (block $break
      (br_if $break (i32.eq (local.get $i) (local.get $length)))
      (local.set $nuc (i32.load8_u (i32.add (local.get $offset) (local.get $i))))
      (local.set $p (i32.add (local.get $offset) (local.get $i)))

      (if       (call $convert (local.get $nuc) (i32.const 65) (i32.const 85) (local.get $p)) (then (nop))          ;; A -> U
      (else (if (call $convert (local.get $nuc) (i32.const 67) (i32.const 71) (local.get $p)) (then (nop))          ;; C -> G
      (else (if (call $convert (local.get $nuc) (i32.const 71) (i32.const 67) (local.get $p)) (then (nop))          ;; G -> C
      (else (if (call $convert (local.get $nuc) (i32.const 84) (i32.const 65) (local.get $p)) (then (nop)))))))))   ;; T -> A
      
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $loop)))

    (return (local.get $offset) (local.get $length))))
