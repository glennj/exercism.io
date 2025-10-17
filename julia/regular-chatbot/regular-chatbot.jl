is_valid_command(msg) = occursin(r"^chatbot\b"i, msg)

remove_emoji(msg) = replace(msg, r"\b emoji \d+"x => "")

check_phone_number(number) =
    occursin(r"^\(\+\d{2}\) \d{3}-\d{3}-\d{3}$", number) ?
        "Thanks! You can now download me to your phone." :
        "Oops, it seems like I can't reach out to $number"

getURL(msg) = [m.match for m in eachmatch(r"\w+(?:\.\w+)+", msg)]

function nice_to_meet_you(str)
    #m = match(r"(?<last>\w+), (?<first>\w+)", str)
    #"Nice to meet you, $(m["first"]) $(m["last"])"

    "Nice to meet you, $(replace(str, r"(\w+), (\w+)" => s"\2 \1"))"
end
