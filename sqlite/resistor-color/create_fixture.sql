-- The color_code has specific colors and needs the result filled in.
DROP TABLE IF EXISTS "color_code";

CREATE TABLE "color_code" ("color" TEXT, "result" INT);

.mode csv
.import ./data.csv color_code
