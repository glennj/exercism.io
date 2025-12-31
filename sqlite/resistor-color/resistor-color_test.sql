-- Setup test table and read in student solution:
.read ./test_setup.sql
-- Test cases:
INSERT INTO
  "test_color_code" (name, uuid, color, result)
VALUES
  (
    'Color codes -> Black',
    '49eb31c5-10a8-4180-9f7f-fea632ab87ef',
    'black',
    0
  ),
  (
    'Color codes -> White',
    '0a4df94b-92da-4579-a907-65040ce0b3fc',
    'white',
    9
  ),
  (
    'Color codes -> Orange',
    '5f81608d-f36f-4190-8084-f45116b6f380',
    'orange',
    3
  );

-- Comparison of user input and the tests updates the status for each test:
UPDATE "test_color_code"
SET
  status = 'pass'
FROM
  (
    SELECT
      color,
      result
    FROM
      color_code
  ) AS actual
WHERE
  (actual.color, actual.result) = (test_color_code.color, test_color_code.result);

-- Write results and debug info:
.read ./test_reporter.sql
