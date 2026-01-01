DROP TABLE IF EXISTS "bottle-song";

CREATE TABLE "bottle-song" (
  start_bottles INTEGER NOT NULL,
  take_down INTEGER NOT NULL,
  result TEXT
);

.mode csv
.import ./data.csv "bottle-song"
