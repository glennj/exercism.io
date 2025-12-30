DROP TABLE IF EXISTS gigasecond;

CREATE TABLE "gigasecond" ("moment" TEXT, "result" TEXT);

-- Note: the CSV file contain literal tab, newline, carriage returns.
.mode csv
.import ./data.csv gigasecond
