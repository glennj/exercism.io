BEGIN {
    RS = @/[.!?]($|[[:space:]]+)/
    words = chars = 0
}

function trimright(s) {
    return gensub(/[[:space:]]+$/, "", 1, s)
}

# Re-add the sentence-ending puncuation.
# RT is the text that matches RS for this record
{ $0 = trimright($0 RT) }

NR == 1 { print "The text is:" }
NR  < 4 { print gensub(/^(.{50}).+/, "\\1...", 1, $0) }
NR == 4 { print "..." }

{
    words += NF
    for (i = 1; i <= NF; i++)
        chars += length($i)
}

END {
    sentences = NR
    score = 4.71 * chars / words + 0.5 * words / sentences - 21.43
    rounded = int(score + 0.5)

    if (rounded <= 1)
        age = "5-6"
    else if (rounded >= 14)
        age = "18-22"
    else
        age = sprintf("%d-%d", rounded + 4, rounded + 5)

    print "Words:", words
    print "Sentences:", sentences
    print "Characters:", chars
    print "Score:", sprintf("%.2f", score)
    print "This text should be understood by", age, "year-olds."
}
