(module
    (memory (export "mem") 1)

    (func $rotate (param $p i32) (param $shift i32) 
    )

    (func (export "rotate") (param $textOffset i32) (param $textLength i32) (param $shiftKey i32) (result i32 i32)
        (local $i i32)
        (loop $while
            (if (i32.lt_u (local.get $i) (local.get $textLength))
                (then
                    (br $while)
                )
            )
        )
        (return (local.get $textOffset) (local.get $textLength))
    )
)
