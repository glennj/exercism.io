DROP TABLE IF EXISTS "dnd-character";

CREATE TABLE "dnd-character" (
  property TEXT NOT NULL,
  input TEXT NOT NULL,
  strength INTEGER,
  dexterity INTEGER,
  constitution INTEGER,
  intelligence INTEGER,
  wisdom INTEGER,
  charisma INTEGER,
  modifier INTEGER,
  hitpoints INTEGER
);

.mode csv
.import ./data.csv "dnd-character"
UPDATE "dnd-character"
SET
  strength = NULL
WHERE
  strength = '';

UPDATE "dnd-character"
SET
  dexterity = NULL
WHERE
  dexterity = '';

UPDATE "dnd-character"
SET
  constitution = NULL
WHERE
  constitution = '';

UPDATE "dnd-character"
SET
  intelligence = NULL
WHERE
  intelligence = '';

UPDATE "dnd-character"
SET
  wisdom = NULL
WHERE
  wisdom = '';

UPDATE "dnd-character"
SET
  charisma = NULL
WHERE
  charisma = '';

UPDATE "dnd-character"
SET
  modifier = NULL
WHERE
  modifier = '';

UPDATE "dnd-character"
SET
  hitpoints = NULL
WHERE
  hitpoints = '';
