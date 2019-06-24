function secret_handshake(code::Integer)
    actions = [ "wink", "double blink", "close your eyes", "jump" ]

    f = code & 0b1_0000 ≠ 0 ? reverse : identity

    # first take, looping
    ##handshake = []
    ##for (i, action) in enumerate(actions)
    ##    key = 1 << (i - 1)
    ##    if code & key > 0
    ##        push!(handshake, action)
    ##    end
    ##end

    # second  take, iterables
    ##handshake = map(
    ##    i -> actions[i],
    ##    filter(
    ##        i -> code & (1 << (i-1)) > 0,
    ##        eachindex(actions)
    ##    )
    ##)

    # third take, comprehensions
    handshake =  [
        action
        for (i, action) in enumerate(actions)
        if code & (1 << (i-1)) ≠ 0
    ]

    f(handshake)
end
