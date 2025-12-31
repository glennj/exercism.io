-- Setup test table and read in student solution:
.read ./test_setup.sql
-- Test cases:
INSERT INTO
  tests (name, uuid, dna, expected)
VALUES
  (
    'Empty RNA sequence',
    'b4631f82-c98c-4a2f-90b3-c5c2b6c6f661',
    '',
    ''
  ),
  (
    'RNA complement of cytosine is guanine',
    'a9558a3c-318c-4240-9256-5d5ed47005a6',
    'C',
    'G'
  ),
  (
    'RNA complement of guanine is cytosine',
    '6eedbb5c-12cb-4c8b-9f51-f8320b4dc2e7',
    'G',
    'C'
  ),
  (
    'RNA complement of thymine is adenine',
    '870bd3ec-8487-471d-8d9a-a25046488d3e',
    'T',
    'A'
  ),
  (
    'RNA complement of adenine is uracil',
    'aade8964-02e1-4073-872f-42d3ffd74c5f',
    'A',
    'U'
  ),
  (
    'RNA complement',
    '79ed2757-f018-4f47-a1d7-34a559392dbf',
    'ACGTGGTCTTAA',
    'UGCACCAGAAUU'
  );

-- Comparison of user input and the tests updates the status for each test:
UPDATE tests
SET
  status = 'pass'
FROM
  (
    SELECT
      dna,
      result
    FROM
      "rna-transcription"
  ) AS actual
WHERE
  (actual.dna, actual.result) = (tests.dna, tests.expected);

-- Write results and debug info:
.read ./test_reporter.sql
