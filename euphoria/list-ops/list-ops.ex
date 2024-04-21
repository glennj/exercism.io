public function my_length(sequence seq)
    -- There appears to be no other way to iterate over a sequence
    -- _without_ knowing its length.
    -- `length()` is absolutely a primative operation in Euphoria.
    return length(seq)
end function

public function my_append(sequence seq1, sequence seq2)
    return seq1 & seq2
end function

public function my_concatenate(sequence seq)
    sequence result = {}
    for i = 1 to length(seq) do
        result = my_append(result, seq[i])
    end for
    return result
end function

public function my_reverse(sequence seq)
    sequence result = {}
    for i = 1 to length(seq) do
        result = my_append({seq[i]}, result)
    end for
    return result
end function

public function my_filter(sequence seq, integer fn)
    sequence result = {}
    for i = 1 to length(seq) do
        if call_func(fn, {seq[i]}) then
            result = my_append(result, {seq[i]})
        end if
    end for
    return result
end function

public function my_map(sequence seq, integer fn)
    sequence result = {}
    for i = 1 to length(seq) do
        result = my_append(result, {call_func(fn, {seq[i]})})
    end for
    return result
end function

public function my_foldl(sequence seq, integer fn, object initial)
    object accumulator = initial
    for i = 1 to length(seq) do
        accumulator = call_func(fn, {accumulator, seq[i]})
    end for
    return accumulator
end function

public function my_foldr(sequence seq, integer fn, object initial)
    object accumulator = initial
    for i = length(seq) to 1 by -1 do
        accumulator = call_func(fn, {seq[i], accumulator})
    end for
    return accumulator
end function
