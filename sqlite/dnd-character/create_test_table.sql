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
  property TEXT NOT NULL,
  input TEXT NOT NULL,
  constitution INTEGER,
  modifier INTEGER,
  expected TEXT NOT NULL
);

INSERT INTO
  tests (
    uuid,
    description,
    property,
    input,
    constitution,
    modifier,
    expected
  )
VALUES
  (
    '1e9ae1dc-35bd-43ba-aa08-e4b94c20fa37',
    'ability modifier for score 3 is -4',
    'modifier',
    'score',
    3,
    -4,
    ''
  ),
  (
    'cc9bb24e-56b8-4e9e-989d-a0d1a29ebb9c',
    'ability modifier for score 4 is -3',
    'modifier',
    'score',
    4,
    -3,
    ''
  ),
  (
    '5b519fcd-6946-41ee-91fe-34b4f9808326',
    'ability modifier for score 5 is -3',
    'modifier',
    'score',
    5,
    -3,
    ''
  ),
  (
    'dc2913bd-6d7a-402e-b1e2-6d568b1cbe21',
    'ability modifier for score 6 is -2',
    'modifier',
    'score',
    6,
    -2,
    ''
  ),
  (
    '099440f5-0d66-4b1a-8a10-8f3a03cc499f',
    'ability modifier for score 7 is -2',
    'modifier',
    'score',
    7,
    -2,
    ''
  ),
  (
    'cfda6e5c-3489-42f0-b22b-4acb47084df0',
    'ability modifier for score 8 is -1',
    'modifier',
    'score',
    8,
    -1,
    ''
  ),
  (
    'c70f0507-fa7e-4228-8463-858bfbba1754',
    'ability modifier for score 9 is -1',
    'modifier',
    'score',
    9,
    -1,
    ''
  ),
  (
    '6f4e6c88-1cd9-46a0-92b8-db4a99b372f7',
    'ability modifier for score 10 is 0',
    'modifier',
    'score',
    10,
    0,
    ''
  ),
  (
    'e00d9e5c-63c8-413f-879d-cd9be9697097',
    'ability modifier for score 11 is 0',
    'modifier',
    'score',
    11,
    0,
    ''
  ),
  (
    'eea06f3c-8de0-45e7-9d9d-b8cab4179715',
    'ability modifier for score 12 is +1',
    'modifier',
    'score',
    12,
    1,
    ''
  ),
  (
    '9c51f6be-db72-4af7-92ac-b293a02c0dcd',
    'ability modifier for score 13 is +1',
    'modifier',
    'score',
    13,
    1,
    ''
  ),
  (
    '94053a5d-53b6-4efc-b669-a8b5098f7762',
    'ability modifier for score 14 is +2',
    'modifier',
    'score',
    14,
    2,
    ''
  ),
  (
    '8c33e7ca-3f9f-4820-8ab3-65f2c9e2f0e2',
    'ability modifier for score 15 is +2',
    'modifier',
    'score',
    15,
    2,
    ''
  ),
  (
    'c3ec871e-1791-44d0-b3cc-77e5fb4cd33d',
    'ability modifier for score 16 is +3',
    'modifier',
    'score',
    16,
    3,
    ''
  ),
  (
    '3d053cee-2888-4616-b9fd-602a3b1efff4',
    'ability modifier for score 17 is +3',
    'modifier',
    'score',
    17,
    3,
    ''
  ),
  (
    'bafd997a-e852-4e56-9f65-14b60261faee',
    'ability modifier for score 18 is +4',
    'modifier',
    'score',
    18,
    4,
    ''
  ),
  (
    '4f28f19c-2e47-4453-a46a-c0d365259c14',
    'random ability is within range',
    'ability',
    'random',
    NULL,
    NULL,
    'score >= 3 && score <= 18'
  ),
  (
    '385d7e72-864f-4e88-8279-81a7d75b04ad',
    'random character is valid',
    'character',
    'random',
    NULL,
    NULL,
    '{"strength":"strength >= 3 && strength <= 18","dexterity":"dexterity >= 3 && dexterity <= 18","constitution":"constitution >= 3 && constitution <= 18","intelligence":"intelligence >= 3 && intelligence <= 18","wisdom":"wisdom >= 3 && wisdom <= 18","charisma":"charisma >= 3 && charisma <= 18","hitpoints":"hitpoints == 10 + modifier(constitution)"}'
  ),
  (
    'dca2b2ec-f729-4551-84b9-078876bb4808',
    'each ability is only calculated once',
    'character',
    '',
    NULL,
    NULL,
    '{"strength":"strength == strength","dexterity":"dexterity == dexterity","constitution":"constitution == constitution","intelligence":"intelligence == intelligence","wisdom":"wisdom == wisdom","charisma":"charisma == charisma"}'
  );
