-- Schema: CREATE TABLE "grains" ("task" TEXT, "square" INT, "result" INT);

UPDATE grains
SET result =
    CASE task
    WHEN 'single-square' THEN POW(2, square - 1)
    WHEN 'total' THEN POW(2, 64) - 1
    END
;

/* Apparently the bit shift operators result in signed 64-bit ints.
 *
 * `1 << (square - 1)` gives a test failure for square = 64
 *  > Result for single-square as 64 is <-9223372036854775808> but should be <9.22337203685478e+18>
 *
 * `(1 << 64) - 1` fails for total
 *  > Result for total as 0 is <-1> but should be <1.84467440737096e+19>
 */
