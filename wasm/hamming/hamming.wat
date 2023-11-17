(module
    (memory (export "mem") 1)

    (func $inc (param i32) (result i32)
        (i32.add (local.get 0) (i32.const 1)))

    (func (export "compute")
        (param $firstOffset i32) (param $firstLength i32) (param $secondOffset i32) (param $secondLength i32) (result i32)
        (local $count i32)
        (local $i i32)

        (if (i32.ne (local.get $firstLength) (local.get $secondLength))
            (then (return (i32.const -1))))

        (loop $loop (result i32)
            (if (i32.eq (local.get $i) (local.get $firstLength))
                (then (return (local.get $count))))

            (i32.load8_u (i32.add (local.get $i) (local.get $firstOffset)))
            (i32.load8_u (i32.add (local.get $i) (local.get $secondOffset)))
            (if (i32.ne)
                (then (local.set $count (call $inc (local.get $count)))))

            (local.set $i (call $inc (local.get $i)))
            (br $loop))))
