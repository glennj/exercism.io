-- Schema: CREATE TABLE "darts" ("x" REAL, "y" REAL, score INTEGER);

/* -- 1. straitforward approach

UPDATE darts
SET score = 
    CASE
    WHEN x*x + y*y <=   1 THEN 10
    WHEN x*x + y*y <=  25 THEN  5
    WHEN x*x + y*y <= 100 THEN  1
    ELSE 0
    END
;
*/

/* -- 2. stash the distance in the table

ALTER TABLE darts 
ADD COLUMN distance REAL;

UPDATE darts
SET distance = sqrt(x * x + y * y);

UPDATE darts
SET score = 
    CASE
    WHEN distance <=  1 THEN 10
    WHEN distance <=  5 THEN  5
    WHEN distance <= 10 THEN  1
    ELSE 0
    END
;
*/

/* -- 3. stash the distance _formula_ in the table

ALTER TABLE darts 
ADD COLUMN distance REAL GENERATED AS (sqrt(x * x + y * y));

UPDATE darts
SET score = 
    CASE
    WHEN distance <=  1 THEN 10
    WHEN distance <=  5 THEN  5
    WHEN distance <= 10 THEN  1
    ELSE 0
    END
;
*/

/* -- 4. stash both the distance and score _formulas_ in the table

ALTER TABLE darts ADD COLUMN distance REAL GENERATED AS (sqrt(x * x + y * y));

ALTER TABLE darts DROP COLUMN score;

-- -- IIF() with > 3 args is only available in version 3.49.0 +
-- ALTER TABLE darts ADD COLUMN score INTEGER GENERATED AS (
--     IIF(distance <= 1, 10, distance <= 5, 5, distance <= 10, 1, 0)
-- );
ALTER TABLE darts ADD COLUMN score INTEGER GENERATED AS (
    WHCASE
    WHEN distance <=  1 THEN 10
    WHEN distance <=  5 THEN  5
    WHEN distance <= 10 THEN  1
    ELSE 0
    END
);
*/

-- 5. don't alter the table, and only compute the distances once.
--    credit to @BNAndras

WITH hypot AS (
    SELECT x, y, sqrt(x * x + y * y) AS dist
    FROM darts
)
UPDATE darts
SET score = 
    CASE 
    WHEN dist <= 1 THEN 10
    WHEN dist <= 5 THEN 5
    WHEN dist <= 10 THEN 1
    ELSE 0
    END
FROM hypot
WHERE darts.x = hypot.x AND darts.y = hypot.y
;
