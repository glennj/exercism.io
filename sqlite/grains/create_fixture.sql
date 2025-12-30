DROP TABLE IF EXISTS grains;

CREATE TABLE "grains" (
  "task" TEXT NOT NULL,
  "square" INT NOT NULL,
  "result" INT NOT NULL
);

.mode csv
.import ./data.csv grains
