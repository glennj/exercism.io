(module
    (memory (export "mem") 1)

    (func $get-result (result i32 i32 i32 i32)
        (i32.load8_u (i32.const 0))     ;; storage for A
        (i32.load8_u (i32.const 2))     ;; storage for C
        (i32.load8_u (i32.const 6))     ;; storage for G
        (i32.load8_u (i32.const 19)))   ;; storage for T

    (func $invalid-result (result i32 i32 i32 i32)
        (i32.const -1) (i32.const -1) (i32.const -1) (i32.const -1))

    (func $inc (param i32) (result i32) (i32.add (local.get 0) (i32.const 1)))

    ;; returns true if the location is for a valid nucleotide, false otherwise.
    (func $inc-mem (param i32) (result i32)
        (local.get 0) (i32.const 65) (i32.eq)           ;; A
        (local.get 0) (i32.const 67) (i32.eq) (i32.or)  ;; C
        (local.get 0) (i32.const 71) (i32.eq) (i32.or)  ;; G
        (local.get 0) (i32.const 84) (i32.eq) (i32.or)  ;; T
        (if (then 
            (local.set 0 (i32.sub (local.get 0) (i32.const 65)))
            (i32.store8 (local.get 0) (call $inc (i32.load8_u (local.get 0))))
            (return (i32.const 1))))
        (return (i32.const 0)))


    (func (export "countNucleotides") (param $offset i32) (param $length i32) (result i32 i32 i32 i32)
        (local $i i32)
        (if (i32.ne (i32.const 0) (local.get $length))
            (then
                (loop $while
                    (call $inc-mem (i32.load8_u (i32.add (local.get $offset) (local.get $i))))
                    (if (i32.eqz) (then (return (call $invalid-result))))
                    (local.set $i (call $inc (local.get $i)))
                    (br_if $while (i32.lt_u (local.get $i) (local.get $length))))))

        (call $get-result)))
