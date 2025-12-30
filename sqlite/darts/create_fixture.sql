DROP TABLE IF EXISTS darts;

CREATE TABLE darts (x REAL, y REAL, score INTEGER);

-- Note: the CSV file contain literal tab, newline, carriage returns.
.mode csv
.import ./data.csv darts
