WITH RECURSIVE
    count_bits(number, remaining, sum) AS (
        SELECT number, number >> 1, number & 1
        FROM "eliuds-eggs"

        UNION

        SELECT number, remaining >> 1, sum + (remaining & 1)
        FROM count_bits
        WHERE remaining > 0
    )
UPDATE "eliuds-eggs"
SET result = sum
FROM count_bits
WHERE "eliuds-eggs".number = count_bits.number
  AND count_bits.remaining = 0
;
