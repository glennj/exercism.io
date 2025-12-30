DROP TABLE IF EXISTS raindrops;

CREATE TABLE "raindrops" ("number" INT, "sound" TEXT);

.mode csv
.import ./data.csv raindrops
