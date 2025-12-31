-- Update message for failed tests to give helpful information:
UPDATE test_color_code as tests
SET
  message = (
    'Result for "' || tests.color || '"' || ' is <' || COALESCE(actual.result, 'NULL') || '> but should be <' || tests.result || '>'
  )
FROM
  (
    SELECT
      color,
      result
    FROM
      color_code
  ) AS actual
WHERE
  actual.color = tests.color
  AND tests.status = 'fail';

.mode json
.once './output.json'
SELECT
  name,
  status,
  message,
  output,
  test_code,
  task_id
FROM
  test_color_code;

-- Display test results in readable form for the student:
.mode table
SELECT
  name,
  status,
  message
FROM
  test_color_code;
