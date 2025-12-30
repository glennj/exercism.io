-- Schema: CREATE TABLE "raindrops" ("number" INT, "sound" TEXT);

WITH drops AS (
    SELECT number
         , IIF(number % 3 = 0, 'Pling', '') AS three
         , IIF(number % 5 = 0, 'Plang', '') AS five
         , IIF(number % 7 = 0, 'Plong', '') AS seven
    FROM raindrops
)
, sounds AS (
    -- apparently concat() arrived after 3.41.x
    --SELECT number, CONCAT(three, five, seven) AS sound
    SELECT number, three || five || seven AS sound
    FROM drops
)
UPDATE raindrops
SET sound = IIF(sounds.sound != '', sounds.sound, raindrops.number)
FROM sounds
WHERE raindrops.number = sounds.number
;
