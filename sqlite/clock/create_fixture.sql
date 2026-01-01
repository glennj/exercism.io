DROP TABLE IF EXISTS clock;

CREATE TABLE clock (
  property TEXT NOT NULL,
  input TEXT NOT NULL, -- json object
  result TEXT
);

.mode csv
.import ./data.csv clock
UPDATE clock
SET
  result = NULL;
