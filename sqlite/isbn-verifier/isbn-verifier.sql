-- Schema: CREATE TABLE "isbn-verifier" (isbn TEXT NOT NULL, result BOOL);

WITH RECURSIVE
    cleaned AS (
        SELECT isbn, REPLACE(isbn, '-', '') AS "cleaned"
        FROM "isbn-verifier"
        -- this glob pattern enforces the length == 10 check
        WHERE cleaned GLOB '[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9X]'
    )
    , summed(isbn, cleaned, i, sum) AS (
        SELECT isbn, cleaned, 0, 0
        FROM cleaned

        UNION ALL

        SELECT isbn, cleaned, i + 1, sum + (10 - i) * IIF(substr(cleaned, i+1, 1) = 'X', 10, substr(cleaned, i+1, 1))
        FROM summed
        WHERE i < 10
    )
UPDATE "isbn-verifier" AS isbn
SET result = summed.sum % 11 == 0
FROM summed
WHERE summed.i = 10
  AND isbn.isbn = summed.isbn
;

UPDATE "isbn-verifier"
SET result = false
WHERE result IS NULL
;
