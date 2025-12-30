-- Setup test table and read in student solution:
.read ./test_setup.sql
-- Test cases:
INSERT INTO
  tests (name, uuid, input, expected)
VALUES
  (
    'no name given',
    '1cf3e15a-a3d7-4a87-aeb3-ba1b43bc8dce',
    '',
    'One for you, one for me.'
  ),
  (
    'a name given',
    'b4c6dbb8-b4fb-42c2-bafd-10785abe7709',
    'Alice',
    'One for Alice, one for me.'
  ),
  (
    'another name given',
    '3549048d-1a6e-4653-9a79-b0bda163e8d5',
    'Bob',
    'One for Bob, one for me.'
  );

-- Comparison of user input and the tests updates the status for each test:
UPDATE tests
SET
  status = 'pass'
FROM
  (
    SELECT
      input,
      response
    FROM
      twofer
  ) AS actual
WHERE
  (actual.input, actual.response) = (tests.input, tests.expected);

-- Write results and debug info:
.read ./test_reporter.sql
