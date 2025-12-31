-- Update message for failed tests to give helpful information:
UPDATE tests
SET
  message = (
    'Result for "' || tests.color1 || '" and "' || tests.color2 || '"' || ' is <' || COALESCE(actual.result, 'NULL') || '> but should be <' || tests.expected || '>'
  )
FROM
  (
    SELECT
      color1,
      color2,
      result
    FROM
      "color_code"
  ) AS actual
WHERE
  (actual.color1, actual.color2) = (tests.color1, tests.color2)
  AND tests.status = 'fail';

-- Save results to ./output.json (needed by the online test-runner)
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
  tests;

-- Display test results in readable form for the student:
.mode table
SELECT
  name,
  status,
  message
FROM
  tests;
