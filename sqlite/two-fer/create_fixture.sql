DROP TABLE IF EXISTS twofer;

CREATE TABLE "twofer" ("input" TEXT, "response" TEXT);

-- Note: the CSV file contain literal tab, newline, carriage returns.
.mode csv
.import ./data.csv twofer
