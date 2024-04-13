namespace char

public function is_ascii_letter(atom char)
    return is_ascii_upper(char) or is_ascii_lower(char)
end function

public function is_ascii_upper(atom char)
    return 'A' <= char and char <= 'Z'
end function

public function is_ascii_lower(atom char)
    return 'a' <= char and char <= 'z'
end function
