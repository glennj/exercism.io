-- Schema: CREATE TABLE "space-age" (
--             planet  TEXT    NOT NULL,
--             seconds INTEGER NOT NULL,
--             result  REAL
--         );

/*
CREATE TEMP TABLE planet_year (
    planet TEXT
  , relative_to_earth REAL
);

INSERT INTO planet_year VALUES
    ('Mercury',   0.2408467)
  , ('Venus',     0.61519726)
  , ('Earth',     1.0)
  , ('Mars',      1.8808158)
  , ('Jupiter',  11.862615)
  , ('Saturn',   29.447498)
  , ('Uranus',   84.016846)
  , ('Neptune', 164.79132)
;
*/

-- I did not know you could specify a "table literal" with a VALUES list
WITH planet_year(planet, relative_to_earth) AS (
    VALUES
        ('Mercury',   0.2408467)
      , ('Venus',     0.61519726)
      , ('Earth',     1.0)
      , ('Mars',      1.8808158)
      , ('Jupiter',  11.862615)
      , ('Saturn',   29.447498)
      , ('Uranus',   84.016846)
      , ('Neptune', 164.79132)
)
, earth_year AS (
    SELECT 31557600 AS in_seconds
)
UPDATE "space-age" AS s
SET result = ROUND(seconds / (earth_year.in_seconds * planet_year.relative_to_earth), 2)
FROM planet_year, earth_year
WHERE s.planet = planet_year.planet
;
