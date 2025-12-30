DROP TABLE IF EXISTS "difference-of-squares";

CREATE TABLE "difference-of-squares" ("number" INT, "property" TEXT, "result" INT);

.mode csv
.import ./data.csv difference-of-squares
