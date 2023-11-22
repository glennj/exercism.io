(module
    (memory (export "mem") 1)

    (data (i32.const 200) "black,brown,red,orange,yellow,green,blue,violet,grey,white")
    (global $colorsLength i32 (i32.const 58))

    ;; offsets
    (func $set-offsets
        (i32.store8 (i32.const 0) (i32.const 200))  ;; "black"
        (i32.store8 (i32.const 1) (i32.const 206))  ;; "brown"
        (i32.store8 (i32.const 2) (i32.const 212))  ;; "red"
        (i32.store8 (i32.const 3) (i32.const 216))  ;; "orange"
        (i32.store8 (i32.const 4) (i32.const 223))  ;; "yellow"
        (i32.store8 (i32.const 5) (i32.const 230))  ;; "green"
        (i32.store8 (i32.const 6) (i32.const 236))  ;; "blue"
        (i32.store8 (i32.const 7) (i32.const 241))  ;; "violet"
        (i32.store8 (i32.const 8) (i32.const 248))  ;; "grey"
        (i32.store8 (i32.const 9) (i32.const 253))) ;; "white"

    (start $set-offsets)


    ;; Utility functions
    (func $inc (param i32) (result i32) (i32.add (i32.const 1) (local.get 0)))

    (func $str-eq (param $off1 i32) (param $off2 i32) (param $len i32) (result i32)
        (local $i i32)
        (loop $str-eq-loop (result i32)
            ;; if we reach $len, then all chars are equal, thus strings are equal
            (local.get $i)
            (local.get $len)
            (if (i32.eq) (then (return (i32.const 1))))

            ;; compare characters
            (i32.load8_u (i32.add (local.get $off1) (local.get $i)))
            (i32.load8_u (i32.add (local.get $off2) (local.get $i)))
            (if (i32.ne) (then (return (i32.const 0))))

            (local.set $i (call $inc (local.get $i)))
            (br $str-eq-loop)))


    ;; Return buffer of comma separated colors
    (func (export "colors") (result i32 i32)
        (return (i32.load8_u (i32.const 0)) (global.get $colorsLength)))


    ;; Given a valid resistor color, returns the associated value
    (func (export "colorCode") (param $offset i32) (param $len i32) (result i32)
        (local $code i32)
        (loop $loop (result i32)
            (if (i32.eq (local.get $code) (i32.const 10))     ;; not found
                (then (return (i32.const -1))))

            ;; the offset for the colour of this $code is stored in memory at offset $code
            (call $str-eq (i32.load8_u (local.get $code)) (local.get $offset) (local.get $len))
            (if (then (return (local.get $code))))

            (local.set $code (call $inc (local.get $code)))
            (br $loop))))
