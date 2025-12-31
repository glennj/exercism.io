WITH t AS (
    SELECT rowid AS r, side_a AS a, side_b AS b, side_c AS c
    FROM triangle
)
, properties AS (
    SELECT r
         , (a > 0 AND b > 0 AND c > 0 AND a + b > c AND a + c > b AND b + c > a)
                                          AS is_valid
         , (a == b AND a == c)            AS is_equilateral
         , (a == b OR a == c OR b == c)   AS is_isosceles
    FROM t
)

UPDATE triangle
SET result = 
    CASE property
    WHEN 'equilateral' THEN is_valid AND is_equilateral
    WHEN 'isosceles'   THEN is_valid AND is_isosceles
    WHEN 'scalene'     THEN is_valid AND NOT is_isosceles
    END
FROM properties
WHERE triangle.rowid = properties.r
; 
