colon_pos(msg) = first(findfirst(":", msg))

message(msg) = strip(msg[(colon_pos(msg) + 1):sizeof(msg)])

log_level(msg) = lowercase(msg[2:(colon_pos(msg) - 2)])

# reformat(msg) = string(message(msg), " (", log_level(msg), ")")
reformat(msg) = "$(message(msg)) ($(log_level(msg)))"
