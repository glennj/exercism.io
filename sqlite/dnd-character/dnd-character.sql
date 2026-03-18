-- Schema:
-- CREATE TABLE "dnd-character" (
--   property     TEXT    NOT NULL,
--   input        TEXT    NOT NULL,
--   strength     INTEGER         ,
--   dexterity    INTEGER         ,
--   constitution INTEGER         ,
--   intelligence INTEGER         ,
--   wisdom       INTEGER         ,
--   charisma     INTEGER         ,
--   modifier     INTEGER         ,
--   hitpoints    INTEGER
-- );
--
-- Task: update the dnd-character table and set the appropriate columns based on the property and the input.

DROP TABLE IF EXISTS d6;
CREATE TEMP TABLE d6 (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    row_id INT NOT NULL,
    characteristic TEXT,
    ability INT
);

DROP TRIGGER IF EXISTS roller;
CREATE TRIGGER roller
AFTER INSERT ON d6 
BEGIN
    UPDATE d6
    SET ability = (
        SELECT SUM(roll) - MIN(roll) FROM (
            SELECT abs(random()) % 6 + 1 AS roll UNION ALL
            SELECT abs(random()) % 6 + 1         UNION ALL
            SELECT abs(random()) % 6 + 1         UNION ALL
            SELECT abs(random()) % 6 + 1
        )
    )
    WHERE id = NEW.id;
END;

INSERT INTO d6 (row_id, characteristic)
SELECT rowid, 'constitution'
FROM "dnd-character"
WHERE property = 'ability';

select * from d6;

UPDATE "dnd-character"
SET modifier = FLOOR(score / 2.0)
WHERE property = 'modifier';
