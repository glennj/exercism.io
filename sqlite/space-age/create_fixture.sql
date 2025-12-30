DROP TABLE IF EXISTS "space-age";

CREATE TABLE "space-age" (
  planet TEXT NOT NULL,
  seconds INTEGER NOT NULL,
  result REAL
);

.mode csv
.import ./data.csv "space-age"
UPDATE "space-age"
SET
  result = NULL;
