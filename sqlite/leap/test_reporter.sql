-- Update message for failed tests to give helpful information:
UPDATE tests
SET
  message = (
    'Result for ' || tests.year || ' is <' || COALESCE(actual.is_leap, 'NULL') || '> but should be <' || tests.result || '>'
  )
FROM
  (
    SELECT
      year,
      is_leap
    FROM
      leap
  ) AS actual
WHERE
  actual.year = tests.year
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
