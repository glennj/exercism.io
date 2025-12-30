-- Setup test table and read in student solution:
.read ./test_setup.sql
-- Test cases:
-- Note: the strings below contain literal tab, newline, carriage returns.
INSERT INTO
  tests (name, uuid, x, y, expected)
VALUES
  (
    'Missed target',
    '9033f731-0a3a-4d9c-b1c0-34a1c8362afb',
    -9,
    9,
    0
  ),
  (
    'On the outer circle',
    '4c9f6ff4-c489-45fd-be8a-1fcb08b4d0ba',
    0,
    10,
    1
  ),
  (
    'On the middle circle',
    '14378687-ee58-4c9b-a323-b089d5274be8',
    -5,
    0,
    5
  ),
  (
    'On the inner circle',
    '849e2e63-85bd-4fed-bc3b-781ae962e2c9',
    0,
    -1,
    10
  ),
  (
    'Exactly on center',
    '1c5ffd9f-ea66-462f-9f06-a1303de5a226',
    0,
    0,
    10
  ),
  (
    'Near the center',
    'b65abce3-a679-4550-8115-4b74bda06088',
    -0.1,
    -0.1,
    10
  ),
  (
    'Just within the inner circle',
    '66c29c1d-44f5-40cf-9927-e09a1305b399',
    0.7,
    0.7,
    10
  ),
  (
    'Just outside the inner circle',
    'd1012f63-c97c-4394-b944-7beb3d0b141a',
    0.8,
    -0.8,
    5
  ),
  (
    'Just within the middle circle',
    'ab2b5666-b0b4-49c3-9b27-205e790ed945',
    -3.5,
    3.5,
    5
  ),
  (
    'Just outside the middle circle',
    '70f1424e-d690-4860-8caf-9740a52c0161',
    -3.6,
    -3.6,
    1
  ),
  (
    'Just within the outer circle',
    'a7dbf8db-419c-4712-8a7f-67602b69b293',
    -7.0,
    7.0,
    1
  ),
  (
    'Just outside the outer circle',
    'e0f39315-9f9a-4546-96e4-a9475b885aa7',
    7.1,
    -7.1,
    0
  ),
  (
    'Asymmetric position between the inner and middle circles',
    '045d7d18-d863-4229-818e-b50828c75d19',
    0.5,
    -4,
    5
  );

-- Comparison of user input and the tests updates the status for each test:
UPDATE tests
SET
  status = 'pass'
FROM
  (
    SELECT
      x,
      y,
      score
    FROM
      darts
  ) AS actual
WHERE
  (actual.x, actual.y, actual.score) = (tests.x, tests.y, tests.expected);

-- Write results and debug info:
.read ./test_reporter.sql
