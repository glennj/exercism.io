;; my indentation style is all over the map here

(module
    (memory (export "mem") 1)

    (func $is-upper (param i32) (result i32)
        (i32.ge_u (local.get 0) (i32.const 65))
        (i32.le_u (local.get 0) (i32.const 90))
        (i32.and))

    (func $is-lower (param i32) (result i32)
        (i32.ge_u (local.get 0) (i32.const 97))
        (i32.le_u (local.get 0) (i32.const 122))
        (i32.and))

    (func $rotate-char (param $c i32) (param $shift i32) (param $offset i32) (result i32)
        (i32.add
            (i32.rem_u
                (i32.add
                    (i32.sub
                        (local.get $c)
                        (local.get $offset))
                    (local.get $shift))
                (i32.const 26))
            (local.get $offset)))

    (func $rotate (param $p i32) (param $shift i32) 
        (local $c i32)
        (local.set $c (i32.load8_u (local.get $p)))

        (if (call $is-upper (local.get $c)) (then
            (i32.store8 (local.get $p)
                        (call $rotate-char (local.get $c) (local.get $shift) (i32.const 65))))

        (else (if (call $is-lower (local.get $c)) (then
            (i32.store8 (local.get $p)
                        (call $rotate-char (local.get $c) (local.get $shift) (i32.const 97))))))))


    (func (export "rotate") (param $textOffset i32) (param $textLength i32) (param $shiftKey i32) (result i32 i32)
        (local $i i32)
        (block $break
            (loop $while
                (i32.eq (local.get $i) (local.get $textLength))
                (if (then (br $break)))

                (call $rotate
                    (i32.add (local.get $textOffset) (local.get $i))
                    (local.get $shiftKey))
                (local.set $i (i32.add (local.get $i) (i32.const 1)))
                (br $while)))
        (return (local.get $textOffset) (local.get $textLength))))
