BEGIN { true = 1; false = 0 }

{ hands[$0] = value() }

END {
    PROCINFO["sorted_in"] = "@val_str_desc"
    for (hand in hands) {
        winning_value = hands[hand]
        break
    }
    for (hand in hands) {
        if (hands[hand] == winning_value)
            print hand
    }
}

# ------------------------------------------------------------
# This function encodes the poker hand into a 6-character hex string.
# - the first character is hand "category":
#     - 5-of-a-kind is category 10; no-pairs is category 1
# - the rest of the characters represent the relevant cards to compare hands
#     - a straight only needs the largest card in the straight for comparison
#     - 2-pair needs the card in the high pair, the card in the low pair, and the other card.
# - then 2 hands can be compared by getting the largest string value.
function value(    faces, suits, i, a, straight, flush, val) {
    for (i = 1; i <= NF; i++) {
        match($i, /^(.+)([SHDC])$/, a)
        switch (a[1]) {
            case "A": faces[i] = 14; break
            case "K": faces[i] = 13; break
            case "Q": faces[i] = 12; break
            case "J": faces[i] = 11; break
            default:  faces[i] = 0 + a[1]
        }
        suits[i] = a[2]
    }

    asort(faces, faces, "@val_num_desc")

    straight = is_straight(faces)
    flush = is_flush(suits)

    if (val = five_of_a_kind(faces, 10)) return val
    if (flush && straight)               return hand_value(9, faces[1])
    if (val = four_of_a_kind(faces, 8))  return val
    if (val = full_house(faces, 7))      return val
    if (flush)
        return hand_value(6, faces[1], faces[2], faces[3], faces[4], faces[5])
    if (straight)                        return hand_value(5, faces[1])
    if (val = three_of_a_kind(faces, 4)) return val
    if (val = two_pair(faces, 3))        return val
    if (val = one_pair(faces, 2))        return val
    return hand_value(1, faces[1], faces[2], faces[3], faces[4], faces[5])
}

function hand_value(category, f1, f2, f3, f4, f5) {
    return sprintf("%X%X%X%X%X%X", 
            category,
            f1 + 0, f2 + 0, f3 + 0, f4 + 0, f5 + 0)
}

function is_straight(faces,     i) {
    # is it an ace-low straight?
    if (faces[1] == 14 && faces[2] == 5 && faces[3] == 4 && faces[4] == 3 && faces[5] == 2) {
        # if so, rewrite the sorted value of the hand
        for (i = 1; i <= 5; i++) faces[i] = 6 - i
        return true
    }
    for (i = 2; i <= 5; i++) {
        if (faces[i - 1] != faces[i] + 1) return false
    }
    return true
}

function is_flush(suits) {
    for (i = 2; i <= 5; i++) {
        if (suits[i - 1] != suits[i]) return false
    }
    return true
}

# ------------------------------------------------------------
function five_of_a_kind(faces, category) {
    if (faces[1] == faces[5]) return hand_value(category, faces[1])
    return false
}

function four_of_a_kind(faces, category) {
    if (faces[1] == faces[4]) return hand_value(category, faces[1], faces[5])
    if (faces[2] == faces[5]) return hand_value(category, faces[2], faces[1])
    return false
}

function full_house(faces, category) {
    if (faces[1] == faces[3] && faces[4] == faces[5])
        return hand_value(category, faces[1], faces[4])
    if (faces[1] == faces[2] && faces[3] == faces[5])
        return hand_value(category, faces[3], faces[1])
    return false
}

function three_of_a_kind(faces, category) {
    if (faces[1] == faces[3]) return hand_value(category, faces[1], faces[4], faces[5])
    if (faces[2] == faces[4]) return hand_value(category, faces[2], faces[1], faces[5])
    if (faces[3] == faces[5]) return hand_value(category, faces[2], faces[1], faces[2])
    return false
}

function two_pair(faces, category) {
    if (faces[1] == faces[2] && faces[3] == faces[4])
        return hand_value(category, faces[1], faces[3], faces[5])
    if (faces[1] == faces[2] && faces[4] == faces[5])
        return hand_value(category, faces[1], faces[4], faces[3])
    if (faces[2] == faces[3] && faces[4] == faces[5])
        return hand_value(category, faces[2], faces[4], faces[1])
    return false
}

function one_pair(faces, category) {
    if (faces[1] == faces[2])
        return hand_value(category, faces[1], faces[3], faces[4], faces[5])
    if (faces[2] == faces[3])
        return hand_value(category, faces[2], faces[1], faces[4], faces[5])
    if (faces[3] == faces[4])
        return hand_value(category, faces[3], faces[1], faces[2], faces[5])
    if (faces[4] == faces[5])
        return hand_value(category, faces[4], faces[1], faces[2], faces[3])
    return false
}
