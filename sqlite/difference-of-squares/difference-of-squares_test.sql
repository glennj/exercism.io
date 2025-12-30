-- Setup test table and read in student solution:
.read ./test_setup.sql
-- Test cases:
-- Note: the strings below _may_ contain literal tab, newline, or carriage returns.
INSERT INTO
  tests (name, uuid, number, property, expected)
VALUES
  (
    'square of sum 1',
    'e46c542b-31fc-4506-bcae-6b62b3268537',
    1,
    'squareOfSum',
    1
  ),
  (
    'square of sum 5',
    '9b3f96cb-638d-41ee-99b7-b4f9c0622948',
    5,
    'squareOfSum',
    225
  ),
  (
    'square of sum 100',
    '54ba043f-3c35-4d43-86ff-3a41625d5e86',
    100,
    'squareOfSum',
    25502500
  ),
  (
    'sum of squares 1',
    '01d84507-b03e-4238-9395-dd61d03074b5',
    1,
    'sumOfSquares',
    1
  ),
  (
    'sum of squares 5',
    'c93900cd-8cc2-4ca4-917b-dd3027023499',
    5,
    'sumOfSquares',
    55
  ),
  (
    'sum of squares 100',
    '94807386-73e4-4d9e-8dec-69eb135b19e4',
    100,
    'sumOfSquares',
    338350
  ),
  (
    'difference of squares 1',
    '44f72ae6-31a7-437f-858d-2c0837adabb6',
    1,
    'differenceOfSquares',
    0
  ),
  (
    'difference of squares 5',
    '005cb2bf-a0c8-46f3-ae25-924029f8b00b',
    5,
    'differenceOfSquares',
    170
  ),
  (
    'difference of squares 100',
    'b1bf19de-9a16-41c0-a62b-1f02ecc0b036',
    100,
    'differenceOfSquares',
    25164150
  );

-- Comparison of user input and the tests updates the status for each test:
UPDATE tests
SET
  status = 'pass'
FROM
  (
    SELECT
      number,
      property,
      result
    FROM
      'difference-of-squares'
  ) AS actual
WHERE
  (actual.number, actual.property, actual.result) = (tests.number, tests.property, tests.expected);

-- Write results and debug info:
.read ./test_reporter.sql
