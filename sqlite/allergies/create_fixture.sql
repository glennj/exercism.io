DROP TABLE IF EXISTS allergies;

CREATE TABLE "allergies" (
  "task" TEXT,
  "item" TEXT,
  "score" INT NOT NULL,
  "result" TEXT
);

.mode csv
.import ./data.csv allergies
