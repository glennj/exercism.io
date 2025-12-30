DROP TABLE IF EXISTS "leap";

CREATE TABLE "leap" ("year" INT, "is_leap" BOOL);

.mode csv
.import ./data.csv leap
