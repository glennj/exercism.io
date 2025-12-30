DROP TABLE IF EXISTS tests;

CREATE TABLE IF NOT EXISTS tests (
  -- uuid and description are taken from the test.toml file
  uuid TEXT PRIMARY KEY,
  description TEXT NOT NULL,
  -- The following section is needed by the online test-runner
  status TEXT DEFAULT 'fail',
  message TEXT,
  output TEXT,
  test_code TEXT,
  task_id INTEGER DEFAULT NULL,
  -- Here are columns for the actual tests
  sentence TEXT NOT NULL,
  expected BOOLEAN NOT NULL
);

INSERT INTO
  tests (uuid, description, sentence, expected)
VALUES
  (
    '64f61791-508e-4f5c-83ab-05de042b0149',
    'empty sentence',
    '',
    false
  ),
  (
    '74858f80-4a4d-478b-8a5e-c6477e4e4e84',
    'perfect lower case',
    'abcdefghijklmnopqrstuvwxyz',
    true
  ),
  (
    '74858f80-4a4d-478b-8a5e-c6477e4e4e84-2',
    'one less',
    'bcdefghijklmnopqrstuvwxyz',
    false
  ),
  (
    '61288860-35ca-4abe-ba08-f5df76ecbdcd',
    'only lower case',
    'the quick brown fox jumps over the lazy dog',
    true
  ),
  (
    '6564267d-8ac5-4d29-baf2-e7d2e304a743',
    'missing the letter ''x''',
    'a quick movement of the enemy will jeopardize five gunboats',
    false
  ),
  (
    'c79af1be-d715-4cdb-a5f2-b2fa3e7e0de0',
    'missing the letter ''h''',
    'five boxing wizards jump quickly at it',
    false
  ),
  (
    'd835ec38-bc8f-48e4-9e36-eb232427b1df',
    'with underscores',
    'the_quick_brown_fox_jumps_over_the_lazy_dog',
    true
  ),
  (
    '8cc1e080-a178-4494-b4b3-06982c9be2a8',
    'with numbers',
    'the 1 quick brown fox jumps over the 2 lazy dogs',
    true
  ),
  (
    'bed96b1c-ff95-45b8-9731-fdbdcb6ede9a',
    'missing letters replaced by numbers',
    '7h3 qu1ck brown fox jumps ov3r 7h3 lazy dog',
    false
  ),
  (
    '938bd5d8-ade5-40e2-a2d9-55a338a01030',
    'mixed case and punctuation',
    '"Five quacking Zephyrs jolt my wax bed."',
    true
  ),
  (
    '7138e389-83e4-4c6e-8413-1e40a0076951',
    'a-m and A-M are 26 different characters but not a pangram',
    'abcdefghijklm ABCDEFGHIJKLM',
    false
  );
