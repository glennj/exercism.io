-- Setup test table and read in student solution:
.read ./test_setup.sql
-- Test cases:
INSERT INTO
  tests (name, uuid, year, result)
VALUES
  (
    'year not divisible by 4 in common year',
    '6466b30d-519c-438e-935d-388224ab5223',
    2015,
    0
  ),
  (
    'year divisible by 2, not divisible by 4 in common year',
    'ac227e82-ee82-4a09-9eb6-4f84331ffdb0',
    1970,
    0
  ),
  (
    'year divisible by 4, not divisible by 100 in leap year',
    '4fe9b84c-8e65-489e-970b-856d60b8b78e',
    1996,
    1
  ),
  (
    'year divisible by 4 and 5 is still a leap year',
    '7fc6aed7-e63c-48f5-ae05-5fe182f60a5d',
    1960,
    1
  ),
  (
    'year divisible by 100, not divisible by 400 in common year',
    '78a7848f-9667-4192-ae53-87b30c9a02dd',
    2100,
    0
  ),
  (
    'year divisible by 100 but not by 3 is still not a leap year',
    '9d70f938-537c-40a6-ba19-f50739ce8bac',
    1900,
    0
  ),
  (
    'year divisible by 400 is leap year',
    '42ee56ad-d3e6-48f1-8e3f-c84078d916fc',
    2000,
    1
  ),
  (
    'year divisible by 400 but not by 125 is still a leap year',
    '57902c77-6fe9-40de-8302-587b5c27121e',
    2400,
    1
  ),
  (
    'year divisible by 200, not divisible by 400 in common year',
    'c30331f6-f9f6-4881-ad38-8ca8c12520c1',
    1800,
    0
  );

-- Comparison of user input and the tests updates the status for each test:
UPDATE tests
SET
  status = 'pass'
FROM
  (
    SELECT
      year,
      is_leap
    FROM
      leap
  ) AS actual
WHERE
  (actual.year, actual.is_leap) = (tests.year, tests.result);

-- Write results and debug info:
.read ./test_reporter.sql
