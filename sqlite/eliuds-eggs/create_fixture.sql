DROP TABLE IF EXISTS "eliuds-eggs";

CREATE TABLE "eliuds-eggs" ("number" INT, "result" INT);

.mode csv
.import ./data.csv eliuds-eggs
