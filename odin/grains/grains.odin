package grains

SIZE :: 64

Error :: enum {
    None,
    InvalidSquare,
}

square :: proc (n: i8) -> (u64, Error) {
    if n < 1 || n > SIZE {
        return 0, .InvalidSquare
    }

    return 1 << u8(n - 1), .None
}

total :: proc () -> (u64, Error) {
    return (1 << SIZE) - 1, .None
}
