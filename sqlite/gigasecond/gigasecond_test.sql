-- Setup test table and read in student solution:
.read ./test_setup.sql
-- Test cases:
-- Note: the strings below contain literal tab, newline, carriage returns.
INSERT INTO
  tests (name, uuid, moment, result)
VALUES
  (
    'date only specification of time',
    '92fbe71c-ea52-4fac-bd77-be38023cacf7',
    '2011-04-25',
    '2043-01-01T01:46:40'
  ),
  (
    'second test for date only specification of time',
    '6d86dd16-6f7a-47be-9e58-bb9fb2ae1433',
    '1977-06-13',
    '2009-02-19T01:46:40'
  ),
  (
    'third test for date only specification of time',
    '77eb8502-2bca-4d92-89d9-7b39ace28dd5',
    '1959-07-19',
    '1991-03-27T01:46:40'
  ),
  (
    'full time specified',
    'c9d89a7d-06f8-4e28-a305-64f1b2abc693',
    '2015-01-24T22:00:00',
    '2046-10-02T23:46:40'
  ),
  (
    'full time with day roll-over',
    '09d4e30e-728a-4b52-9005-be44a58d9eba',
    '2015-01-24T23:59:59',
    '2046-10-03T01:46:39'
  );

-- Comparison of user input and the tests updates the status for each test:
UPDATE tests
SET
  status = 'pass'
FROM
  (
    SELECT
      moment,
      result
    FROM
      gigasecond
  ) AS actual
WHERE
  (actual.moment, actual.result) = (tests.moment, tests.result);

-- Write results and debug info:
.read ./test_reporter.sql
