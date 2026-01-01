DROP TABLE IF EXISTS bob;

CREATE TABLE "bob" ("input" TEXT, "reply" TEXT);

-- Note: the CSV file contain literal tab, newline, carriage returns.
.mode csv
.import ./data.csv bob
