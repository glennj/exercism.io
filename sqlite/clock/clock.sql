CREATE TEMP TABLE clock_formatter (
    clock_row  INTEGER NOT NULL,
    property   TEXT NOT NULL,
    name       TEXT,
    json       TEXT NOT NULL,
    mins       INTEGER GENERATED AS (60 * (json ->> '$.hour') + (json ->> '$.minute')),
    normalized INTEGER GENERATED AS (
                    (( CASE property
                       WHEN 'add'      THEN mins + (json ->> '$.value')
                       WHEN 'subtract' THEN mins - (json ->> '$.value')
                       ELSE mins
                       END ) % 1440 + 1440) % 1440
               ),
    face       TEXT GENERATED AS (format('%02d:%02d', normalized / 60, normalized % 60))
);

INSERT INTO clock_formatter (clock_row, property, json)
SELECT rowid, property, input
FROM clock
WHERE property IN ('create', 'add', 'subtract');

INSERT INTO clock_formatter (clock_row, property, name, json)
SELECT rowid, property, 'clock1', input -> '$.clock1'
FROM clock
WHERE property = 'equal';

INSERT INTO clock_formatter (clock_row, property, name, json)
SELECT rowid, property, 'clock2', input -> '$.clock2'
FROM clock
WHERE property = 'equal';


-- SELECT * FROM clock_formatter;


UPDATE clock
SET result = face
FROM clock_formatter
WHERE clock.property IN ('create', 'add', 'subtract')
  AND clock.rowid = clock_formatter.clock_row
;


WITH clock1 AS (SELECT clock_row, face FROM clock_formatter WHERE name = 'clock1')
   , clock2 AS (SELECT clock_row, face FROM clock_formatter WHERE name = 'clock2')
UPDATE clock
SET result = clock1.face == clock2.face
FROM clock1, clock2
WHERE clock.property = 'equal'
  AND clock.rowid = clock1.clock_row
  AND clock1.clock_row = clock2.clock_row
;
