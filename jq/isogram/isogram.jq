def between(low; up): low <= . and . <= up;
def is_upper:         between(65; 90);
def letters:          ascii_upcase | explode | map(select(is_upper));

.phrase | letters | length == (unique | length)