DROP TABLE IF EXISTS triangle;

CREATE TABLE triangle (
  property TEXT NOT NULL,
  side_a REAL NOT NULL,
  side_b REAL NOT NULL,
  side_c REAL NOT NULL,
  result BOOLEAN
);

.mode csv
.import ./data.csv triangle
UPDATE triangle
SET
  result = NULL;
