-- CREATE TABLE "color_code" ("color" TEXT, "result" INT);

WITH resistor_bands(color, value) AS (
    VALUES
        ('black',  0),
        ('brown',  1),
        ('red',    2),
        ('orange', 3),
        ('yellow', 4),
        ('green',  5),
        ('blue',   6),
        ('violet', 7),
        ('grey',   8),
        ('white',  9)
    )
UPDATE color_code AS cc
SET result = r.value
FROM resistor_bands AS r
WHERE cc.color = r.color;
