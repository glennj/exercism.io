-- Setup test table and read in student solution:
.read ./test_setup.sql
-- Test cases:
-- Note: the strings below contain literal tab, newline, carriage returns.
INSERT INTO
  tests (uuid, name, input, expected)
VALUES
  (
    'e162fead-606f-437a-a166-d051915cea8e',
    'stating something',
    'Tom-ay-to, tom-aaaah-to.',
    'Whatever.'
  ),
  (
    '73a966dc-8017-47d6-bb32-cf07d1a5fcd9',
    'shouting',
    'WATCH OUT!',
    'Whoa, chill out!'
  ),
  (
    'd6c98afd-df35-4806-b55e-2c457c3ab748',
    'shouting gibberish',
    'FCECDFCAAB',
    'Whoa, chill out!'
  ),
  (
    '8a2e771d-d6f1-4e3f-b6c6-b41495556e37',
    'asking a question',
    'Does this cryogenic chamber make me look fat?',
    'Sure.'
  ),
  (
    '81080c62-4e4d-4066-b30a-48d8d76920d9',
    'asking a numeric question',
    'You are, what, like 15?',
    'Sure.'
  ),
  (
    '2a02716d-685b-4e2e-a804-2adaf281c01e',
    'asking gibberish',
    'fffbbcbeab?',
    'Sure.'
  ),
  (
    'c02f9179-ab16-4aa7-a8dc-940145c385f7',
    'talking forcefully',
    'Hi there!',
    'Whatever.'
  ),
  (
    '153c0e25-9bb5-4ec5-966e-598463658bcd',
    'using acronyms in regular speech',
    'It''s OK if you don''t want to go work for NASA.',
    'Whatever.'
  ),
  (
    'a5193c61-4a92-4f68-93e2-f554eb385ec6',
    'forceful question',
    'WHAT''S GOING ON?',
    'Calm down, I know what I''m doing!'
  ),
  (
    'a20e0c54-2224-4dde-8b10-bd2cdd4f61bc',
    'shouting numbers',
    '1, 2, 3 GO!',
    'Whoa, chill out!'
  ),
  (
    'f7bc4b92-bdff-421e-a238-ae97f230ccac',
    'no letters',
    '1, 2, 3',
    'Whatever.'
  ),
  (
    'bb0011c5-cd52-4a5b-8bfb-a87b6283b0e2',
    'question with no letters',
    '4?',
    'Sure.'
  ),
  (
    '496143c8-1c31-4c01-8a08-88427af85c66',
    'shouting with special characters',
    'ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!',
    'Whoa, chill out!'
  ),
  (
    'e6793c1c-43bd-4b8d-bc11-499aea73925f',
    'shouting with no exclamation mark',
    'I HATE THE DENTIST',
    'Whoa, chill out!'
  ),
  (
    'aa8097cc-c548-4951-8856-14a404dd236a',
    'statement containing question mark',
    'Ending with ? means a question.',
    'Whatever.'
  ),
  (
    '9bfc677d-ea3a-45f2-be44-35bc8fa3753e',
    'non-letters with question',
    ':) ?',
    'Sure.'
  ),
  (
    '8608c508-f7de-4b17-985b-811878b3cf45',
    'prattling on',
    'Wait! Hang on. Are you going to be OK?',
    'Sure.'
  ),
  (
    'bc39f7c6-f543-41be-9a43-fd1c2f753fc0',
    'silence',
    '',
    'Fine. Be that way!'
  ),
  (
    'd6c47565-372b-4b09-b1dd-c40552b8378b',
    'prolonged silence',
    '          ',
    'Fine. Be that way!'
  ),
  (
    '4428f28d-4100-4d85-a902-e5a78cb0ecd3',
    'alternate silence',
    '										',
    'Fine. Be that way!'
  ),
  (
    '2c7278ac-f955-4eb4-bf8f-e33eb4116a15',
    'multiple line question',
    '
Does this cryogenic chamber make
 me look fat?',
    'Sure.'
  ),
  (
    '5371ef75-d9ea-4103-bcfa-2da973ddec1b',
    'starting with whitespace',
    '         hmmmmmmm...',
    'Whatever.'
  ),
  (
    '05b304d6-f83b-46e7-81e0-4cd3ca647900',
    'ending with whitespace',
    'Okay if like my  spacebar  quite a bit?   ',
    'Sure.'
  ),
  (
    '72bd5ad3-9b2f-4931-a988-dce1f5771de2',
    'other whitespace',
    '

 	',
    'Fine. Be that way!'
  ),
  (
    '12983553-8601-46a8-92fa-fcaa3bc4a2a0',
    'non-question ending with whitespace',
    'This is a statement ending with whitespace      ',
    'Whatever.'
  );

-- Comparison of user input and the tests updates the status for each test:
UPDATE tests
SET
  status = 'pass'
FROM
  (
    SELECT
      input,
      reply
    FROM
      bob
  ) AS actual
WHERE
  (actual.input, actual.reply) = (tests.input, tests.expected);

-- Write results and debug info:
.read ./test_reporter.sql
