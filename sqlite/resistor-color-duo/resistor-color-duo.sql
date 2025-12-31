-- Schema: CREATE TABLE "color_code" ("color1" TEXT, "color2" TEXT, "result" INT);

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
SET result = b1.value * 10 + b2.value
FROM resistor_bands AS b1
   , resistor_bands AS b2
WHERE cc.color1 = b1.color
  AND cc.color2 = b2.color
;
