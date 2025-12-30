-- Schema: CREATE TABLE "difference-of-squares" ("number" INT, "property" TEXT, "result" INT);

WITH calculations AS (
    SELECT DISTINCT
           number
         , POW((number * (number + 1) / 2), 2) AS squareOfSum
         , (number * (number + 1) * (2 * number + 1) / 6) AS sumOfSquares
    FROM "difference-of-squares"
)
UPDATE "difference-of-squares" AS d
SET result =
    CASE
    WHEN property = 'squareOfSum'  THEN c.squareOfSum
    WHEN property = 'sumOfSquares' THEN c.sumOfSquares
    ELSE c.squareOfSum - c.sumOfSquares
    END
FROM calculations AS c
WHERE d.number = c.number;
