-- Setup test table and read in student solution:
.read ./test_setup.sql
-- Test cases:
-- Note: the strings below _may_ contain literal tab, newline, or carriage returns.
INSERT INTO
  tests (name, uuid, number, expected)
VALUES
  (
    '0 eggs',
    '559e789d-07d1-4422-9004-3b699f83bca3',
    0,
    0
  ),
  (
    '1 egg',
    '97223282-f71e-490c-92f0-b3ec9e275aba',
    16,
    1
  ),
  (
    '4 eggs',
    '1f8fd18f-26e9-4144-9a0e-57cdfc4f4ff5',
    89,
    4
  ),
  (
    '13 eggs',
    '0c18be92-a498-4ef2-bcbb-28ac4b06cb81',
    2000000000,
    13
  );

-- Comparison of user input and the tests updates the status for each test:
UPDATE tests
SET
  status = 'pass'
FROM
  (
    SELECT
      number,
      result
    FROM
      "eliuds-eggs"
  ) AS actual
WHERE
  (actual.number, actual.result) = (tests.number, tests.expected);

-- Write results and debug info:
.read ./test_reporter.sql
