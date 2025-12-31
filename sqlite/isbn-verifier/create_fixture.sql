DROP TABLE IF EXISTS "isbn-verifier";

CREATE TABLE "isbn-verifier" (isbn TEXT NOT NULL, result BOOL);

.mode csv
.import ./data.csv "isbn-verifier"
UPDATE "isbn-verifier"
SET
  result = NULL;
