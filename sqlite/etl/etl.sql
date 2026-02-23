WITH
    level1(row_id, score, letters) AS (
        SELECT etl.rowid, CAST(j.key AS INT), j.value 
        FROM etl, JSON_EACH(etl.input) j
    )
    , level2 AS (
        SELECT row_id, score, LOWER(j.value) AS letter
        FROM level1, JSON_EACH(level1.letters) j
        ORDER BY letter
    )
    , aggregated AS (
        SELECT row_id, JSON_GROUP_OBJECT(letter, score) AS transformed
        FROM level2
        GROUP BY row_id
    )

UPDATE etl
SET result = transformed
FROM aggregated
WHERE etl.rowid = aggregated.row_id;
