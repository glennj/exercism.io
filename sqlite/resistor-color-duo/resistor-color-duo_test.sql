-- Setup test table and read in student solution:
.read ./test_setup.sql
-- Test cases:
INSERT INTO
  tests (name, uuid, color1, color2, expected)
VALUES
  (
    'Brown and black',
    'ce11995a-5b93-4950-a5e9-93423693b2fc',
    'brown',
    'black',
    10
  ),
  (
    'Blue and grey',
    '7bf82f7a-af23-48ba-a97d-38d59406a920',
    'blue',
    'grey',
    68
  ),
  (
    'Yellow and violet',
    'f1886361-fdfd-4693-acf8-46726fe24e0c',
    'yellow',
    'violet',
    47
  ),
  (
    'White and red',
    'b7a6cbd2-ae3c-470a-93eb-56670b305640',
    'white',
    'red',
    92
  ),
  (
    'Orange and orange',
    '77a8293d-2a83-4016-b1af-991acc12b9fe',
    'orange',
    'orange',
    33
  ),
  (
    'Black and brown, one-digit',
    '4a8ceec5-0ab4-4904-88a4-daf953a5e818',
    'black',
    'brown',
    1
  );

-- Comparison of user input and the tests updates the status for each test:
UPDATE tests
SET
  status = 'pass'
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
  (actual.color1, actual.color2, actual.result) = (tests.color1, tests.color2, tests.expected);

-- Write results and debug info:
.read ./test_reporter.sql
