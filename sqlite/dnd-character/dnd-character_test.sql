-- Create database:
.read ./create_fixture.sql
-- Read user student solution and save any output as markdown in user_output.md:
.mode markdown
.output user_output.md
.read ./dnd-character.sql
.output
-- Create a clean testing environment:
.read ./create_test_table.sql
-- Comparison of user input and the tests updates the status for each test:
UPDATE tests
SET
  status = 'pass'
FROM
  (
    SELECT
      property,
      input,
      constitution,
      modifier
    FROM
      "dnd-character"
    WHERE
      property = 'modifier'
      AND input = 'score'
  ) AS actual
WHERE
  (
    actual.property,
    actual.input,
    actual.constitution,
    actual.modifier
  ) = (
    tests.property,
    tests.input,
    tests.constitution,
    tests.modifier
  );

UPDATE tests
SET
  status = 'pass'
FROM
  (
    SELECT
      property,
      input,
      COALESCE(
        strength,
        dexterity,
        constitution,
        intelligence,
        wisdom,
        charisma
      ) AS score
    FROM
      "dnd-character"
    WHERE
      property = 'ability'
      AND input = 'random'
      AND score BETWEEN 3 AND 18
  ) AS actual
WHERE
  (actual.property, actual.input) = (tests.property, tests.input);

UPDATE tests
SET
  status = 'pass'
FROM
  (
    SELECT
      property,
      input,
      strength,
      dexterity,
      constitution,
      intelligence,
      wisdom,
      charisma,
      modifier,
      hitpoints
    FROM
      "dnd-character"
    WHERE
      property = 'character'
      AND input = 'random'
      AND strength BETWEEN 3 AND 18
      AND dexterity BETWEEN 3 AND 18
      AND constitution BETWEEN 3 AND 18
      AND intelligence BETWEEN 3 AND 18
      AND wisdom BETWEEN 3 AND 18
      AND charisma BETWEEN 3 AND 18
      AND hitpoints = 10 + modifier
  ) AS actual
WHERE
  (actual.property, actual.input) = (tests.property, tests.input);

UPDATE tests
SET
  status = 'pass'
FROM
  (
    SELECT
      a.property,
      a.input,
      a.strength,
      a.dexterity,
      a.constitution,
      a.intelligence,
      a.wisdom,
      a.charisma
    FROM
      "dnd-character" a
      JOIN "dnd-character" b ON (a.property = b.property)
    WHERE
      a.property = 'character'
      AND a.input = ''
      AND b.input = 'random'
      AND a.strength = b.strength
      AND a.dexterity = b.dexterity
      AND a.constitution = b.constitution
      AND a.intelligence = b.intelligence
      AND a.wisdom = b.wisdom
      AND a.charisma = b.charisma
  ) AS actual
WHERE
  (actual.property, actual.input) = (tests.property, tests.input);

-- Update message for failed tests to give helpful information:
UPDATE tests
SET
  message = (
    'Result for "' || PRINTF(
      'property=%s, input=%s, constitution=%d',
      tests.property,
      tests.input,
      tests.constitution
    ) || '"' || ' is <' || COALESCE(actual.modifier, 'NULL') || '> but should be <' || tests.modifier || '>'
  )
FROM
  (
    SELECT
      property,
      input,
      constitution,
      modifier
    FROM
      "dnd-character"
    WHERE
      property = 'modifier'
      AND input = 'score'
  ) AS actual
WHERE
  (
    actual.property,
    actual.input,
    actual.constitution
  ) = (tests.property, tests.input, tests.constitution)
  AND tests.status = 'fail';

UPDATE tests
SET
  message = (
    'Result for "' || PRINTF(
      'property=%s, input=%s',
      tests.property,
      tests.input
    ) || '"' || ' is <' || COALESCE(
      actual.strength,
      actual.dexterity,
      actual.constitution,
      actual.intelligence,
      actual.wisdom,
      actual.charisma,
      'NULL'
    ) || '> but should be <' || tests.expected || '>'
  )
FROM
  (
    SELECT
      property,
      input,
      strength,
      dexterity,
      constitution,
      intelligence,
      wisdom,
      charisma
    FROM
      "dnd-character"
    WHERE
      property = 'ability'
      AND input = 'random'
  ) AS actual
WHERE
  (actual.property, actual.input) = (tests.property, tests.input)
  AND tests.status = 'fail';

UPDATE tests
SET
  message = (
    'Result for "' || PRINTF(
      'property=%s, input=%s',
      tests.property,
      tests.input
    ) || '"' || ' is <' || JSON_OBJECT(
      'strength',
      actual.strength,
      'dexterity',
      actual.dexterity,
      'constitution',
      actual.constitution,
      'intelligence',
      actual.intelligence,
      'wisdom',
      actual.wisdom,
      'charisma',
      actual.charisma,
      'modifier',
      actual.modifier,
      'hitpoints',
      actual.hitpoints
    ) || '> but should be <' || tests.expected || '>'
  )
FROM
  (
    SELECT
      property,
      input,
      strength,
      dexterity,
      constitution,
      intelligence,
      wisdom,
      charisma,
      modifier,
      hitpoints
    FROM
      "dnd-character"
    WHERE
      property = 'character'
      AND input = 'random'
  ) AS actual
WHERE
  (actual.property, actual.input) = (tests.property, tests.input)
  AND tests.status = 'fail';

UPDATE tests
SET
  message = (
    'Result for "' || PRINTF(
      'property=%s, input=%s',
      tests.property,
      tests.input
    ) || '"' || ' is <' || JSON_OBJECT(
      'strength',
      actual.strength,
      'dexterity',
      actual.dexterity,
      'constitution',
      actual.constitution,
      'intelligence',
      actual.intelligence,
      'wisdom',
      actual.wisdom,
      'charisma',
      actual.charisma
    ) || '> but should be <' || tests.expected || '>'
  )
FROM
  (
    SELECT
      property,
      input,
      strength,
      dexterity,
      constitution,
      intelligence,
      wisdom,
      charisma
    FROM
      "dnd-character"
    WHERE
      property = 'character'
      AND input = ''
  ) AS actual
WHERE
  (actual.property, actual.input) = (tests.property, tests.input)
  AND tests.status = 'fail';

-- Save results to ./output.json (needed by the online test-runner)
.mode json
.once './output.json'
SELECT
  description,
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
  description,
  status,
  message
FROM
  tests;
