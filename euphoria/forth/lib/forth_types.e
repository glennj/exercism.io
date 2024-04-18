public enum OK, ERR

public type result(sequence data)
    return error(data) or ok(data)
end type

public type ok(sequence data)
    return length(data) > 0 and data[1] = OK
end type

public type error(sequence data)
    return length(data) = 2 and data[1] = ERR
end type

public function errmsg(error e)
    return e[2]
end function

public function result_value(ok o)
    return o[2]
end function
