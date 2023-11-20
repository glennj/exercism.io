(module
    (memory (export "mem") 1)

    (data (i32.const 0) "black,brown,red,orange,yellow,green,blue,violet,grey,white")
    (global $colorsOffset i32 (i32.const 0))
    (global $colorsLength i32 (i32.const 58))

    ;; offsets
    (global $black  i32 (i32.const 0))
    (global $brown  i32 (i32.const 6))
    (global $red    i32 (i32.const 12))
    (global $orange i32 (i32.const 16))
    (global $yellow i32 (i32.const 23))
    (global $green  i32 (i32.const 30))
    (global $blue   i32 (i32.const 36))
    (global $violet i32 (i32.const 41))
    (global $grey   i32 (i32.const 48))
    (global $white  i32 (i32.const 53))

    
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
        (return (global.get $colorsOffset) (global.get $colorsLength)))


    ;; Given a valid resistor color, returns the associated value
    (func (export "colorCode") (param $offset i32) (param $len i32) (result i32)

        (call $str-eq (global.get $black) (local.get $offset) (local.get $len))
        (if (then (return (i32.const 0))))

        (call $str-eq (global.get $brown) (local.get $offset) (local.get $len))
        (if (then (return (i32.const 1))))

        (call $str-eq (global.get $red) (local.get $offset) (local.get $len))
        (if (then (return (i32.const 2))))

        (call $str-eq (global.get $orange) (local.get $offset) (local.get $len))
        (if (then (return (i32.const 3))))

        (call $str-eq (global.get $yellow) (local.get $offset) (local.get $len))
        (if (then (return (i32.const 4))))

        (call $str-eq (global.get $green) (local.get $offset) (local.get $len))
        (if (then (return (i32.const 5))))

        (call $str-eq (global.get $blue) (local.get $offset) (local.get $len))
        (if (then (return (i32.const 6))))

        (call $str-eq (global.get $violet) (local.get $offset) (local.get $len))
        (if (then (return (i32.const 7))))

        (call $str-eq (global.get $grey) (local.get $offset) (local.get $len))
        (if (then (return (i32.const 8))))

        (call $str-eq (global.get $white) (local.get $offset) (local.get $len))
        (if (then (return (i32.const 9))))

        (return (i32.const -1))))
