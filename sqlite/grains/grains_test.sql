-- Setup test table and read in student solution:
.read ./test_setup.sql
-- Test cases:
-- Note: the strings below may contain literal tab, newline, carriage returns.
INSERT INTO
  tests (name, uuid, task, square, expected)
VALUES
  (
    'grains on square 1',
    '9fbde8de-36b2-49de-baf2-cd42d6f28405',
    'single-square',
    1,
    1
  ),
  (
    'grains on square 2',
    'ee1f30c2-01d8-4298-b25d-c677331b5e6d',
    'single-square',
    2,
    2
  ),
  (
    'grains on square 3',
    '10f45584-2fc3-4875-8ec6-666065d1163b',
    'single-square',
    3,
    4
  ),
  (
    'grains on square 4',
    'a7cbe01b-36f4-4601-b053-c5f6ae055170',
    'single-square',
    4,
    8
  ),
  (
    'grains on square 16',
    'c50acc89-8535-44e4-918f-b848ad2817d4',
    'single-square',
    16,
    32768
  ),
  (
    'grains on square 32',
    'acd81b46-c2ad-4951-b848-80d15ed5a04f',
    'single-square',
    32,
    2147483648
  ),
  (
    'grains on square 64',
    'c73b470a-5efb-4d53-9ac6-c5f6487f227b',
    'single-square',
    64,
    9223372036854775808
  ),
  (
    'returns the total number of grains on the board',
    '6eb07385-3659-4b45-a6be-9dc474222750',
    'total',
    0,
    18446744073709551615
  );

-- Comparison of user input and the tests updates the status for each test:
UPDATE tests
SET
  status = 'pass'
FROM
  (
    SELECT
      task,
      square,
      result
    FROM
      grains
  ) AS actual
WHERE
  (actual.task, actual.square, actual.result) = (tests.task, tests.square, tests.expected);

-- Write results and debug info:
.read ./test_reporter.sql
