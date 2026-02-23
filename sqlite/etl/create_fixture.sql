DROP TABLE IF EXISTS "etl";

CREATE TABLE "etl" ("input" TEXT, "result" TEXT);

.mode csv
.import ./data.csv etl
