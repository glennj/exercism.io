DROP TABLE IF EXISTS pangram;

CREATE TABLE pangram (sentence TEXT NOT NULL, result BOOLEAN);

.mode csv
.import ./data.csv pangram
UPDATE pangram
SET
  result = NULL;
