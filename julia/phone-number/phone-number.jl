function clean(phone_number)
    m = match(
        r"^1?((?:[2-9]\d\d){2}\d{4})$", 
        filter(isdigit, phone_number)
    )
    isnothing(m) ? m : m.captures[1]
end
