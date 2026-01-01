-- Setup test table and read in student solution:
.read ./test_setup.sql
-- Test cases:
-- Note: the strings below contain literal tab, newline, carriage returns.
INSERT INTO
  tests (name, uuid, task, item, score, expected)
VALUES
  (
    'not allergic to anything',
    '17fc7296-2440-4ac4-ad7b-d07c321bc5a0',
    'allergicTo',
    'eggs',
    0,
    'false'
  ),
  (
    'allergic only to eggs',
    '07ced27b-1da5-4c2e-8ae2-cb2791437546',
    'allergicTo',
    'eggs',
    1,
    'true'
  ),
  (
    'allergic to eggs and something else',
    '5035b954-b6fa-4b9b-a487-dae69d8c5f96',
    'allergicTo',
    'eggs',
    3,
    'true'
  ),
  (
    'allergic to something, but not eggs',
    '64a6a83a-5723-4b5b-a896-663307403310',
    'allergicTo',
    'eggs',
    2,
    'false'
  ),
  (
    'allergic to everything',
    '90c8f484-456b-41c4-82ba-2d08d93231c6',
    'allergicTo',
    'eggs',
    255,
    'true'
  ),
  (
    'not allergic to anything',
    'd266a59a-fccc-413b-ac53-d57cb1f0db9d',
    'allergicTo',
    'peanuts',
    0,
    'false'
  ),
  (
    'allergic only to peanuts',
    'ea210a98-860d-46b2-a5bf-50d8995b3f2a',
    'allergicTo',
    'peanuts',
    2,
    'true'
  ),
  (
    'allergic to peanuts and something else',
    'eac69ae9-8d14-4291-ac4b-7fd2c73d3a5b',
    'allergicTo',
    'peanuts',
    7,
    'true'
  ),
  (
    'allergic to something, but not peanuts',
    '9152058c-ce39-4b16-9b1d-283ec6d25085',
    'allergicTo',
    'peanuts',
    5,
    'false'
  ),
  (
    'allergic to everything',
    'd2d71fd8-63d5-40f9-a627-fbdaf88caeab',
    'allergicTo',
    'peanuts',
    255,
    'true'
  ),
  (
    'not allergic to anything',
    'b948b0a1-cbf7-4b28-a244-73ff56687c80',
    'allergicTo',
    'shellfish',
    0,
    'false'
  ),
  (
    'allergic only to shellfish',
    '9ce9a6f3-53e9-4923-85e0-73019047c567',
    'allergicTo',
    'shellfish',
    4,
    'true'
  ),
  (
    'allergic to shellfish and something else',
    'b272fca5-57ba-4b00-bd0c-43a737ab2131',
    'allergicTo',
    'shellfish',
    14,
    'true'
  ),
  (
    'allergic to something, but not shellfish',
    '21ef8e17-c227-494e-8e78-470a1c59c3d8',
    'allergicTo',
    'shellfish',
    10,
    'false'
  ),
  (
    'allergic to everything',
    'cc789c19-2b5e-4c67-b146-625dc8cfa34e',
    'allergicTo',
    'shellfish',
    255,
    'true'
  ),
  (
    'not allergic to anything',
    '651bde0a-2a74-46c4-ab55-02a0906ca2f5',
    'allergicTo',
    'strawberries',
    0,
    'false'
  ),
  (
    'allergic only to strawberries',
    'b649a750-9703-4f5f-b7f7-91da2c160ece',
    'allergicTo',
    'strawberries',
    8,
    'true'
  ),
  (
    'allergic to strawberries and something else',
    '50f5f8f3-3bac-47e6-8dba-2d94470a4bc6',
    'allergicTo',
    'strawberries',
    28,
    'true'
  ),
  (
    'allergic to something, but not strawberries',
    '23dd6952-88c9-48d7-a7d5-5d0343deb18d',
    'allergicTo',
    'strawberries',
    20,
    'false'
  ),
  (
    'allergic to everything',
    '74afaae2-13b6-43a2-837a-286cd42e7d7e',
    'allergicTo',
    'strawberries',
    255,
    'true'
  ),
  (
    'not allergic to anything',
    'c49a91ef-6252-415e-907e-a9d26ef61723',
    'allergicTo',
    'tomatoes',
    0,
    'false'
  ),
  (
    'allergic only to tomatoes',
    'b69c5131-b7d0-41ad-a32c-e1b2cc632df8',
    'allergicTo',
    'tomatoes',
    16,
    'true'
  ),
  (
    'allergic to tomatoes and something else',
    '1ca50eb1-f042-4ccf-9050-341521b929ec',
    'allergicTo',
    'tomatoes',
    56,
    'true'
  ),
  (
    'allergic to something, but not tomatoes',
    'e9846baa-456b-4eff-8025-034b9f77bd8e',
    'allergicTo',
    'tomatoes',
    40,
    'false'
  ),
  (
    'allergic to everything',
    'b2414f01-f3ad-4965-8391-e65f54dad35f',
    'allergicTo',
    'tomatoes',
    255,
    'true'
  ),
  (
    'not allergic to anything',
    '978467ab-bda4-49f7-b004-1d011ead947c',
    'allergicTo',
    'chocolate',
    0,
    'false'
  ),
  (
    'allergic only to chocolate',
    '59cf4e49-06ea-4139-a2c1-d7aad28f8cbc',
    'allergicTo',
    'chocolate',
    32,
    'true'
  ),
  (
    'allergic to chocolate and something else',
    'b0a7c07b-2db7-4f73-a180-565e07040ef1',
    'allergicTo',
    'chocolate',
    112,
    'true'
  ),
  (
    'allergic to something, but not chocolate',
    'f5506893-f1ae-482a-b516-7532ba5ca9d2',
    'allergicTo',
    'chocolate',
    80,
    'false'
  ),
  (
    'allergic to everything',
    '02debb3d-d7e2-4376-a26b-3c974b6595c6',
    'allergicTo',
    'chocolate',
    255,
    'true'
  ),
  (
    'not allergic to anything',
    '17f4a42b-c91e-41b8-8a76-4797886c2d96',
    'allergicTo',
    'pollen',
    0,
    'false'
  ),
  (
    'allergic only to pollen',
    '7696eba7-1837-4488-882a-14b7b4e3e399',
    'allergicTo',
    'pollen',
    64,
    'true'
  ),
  (
    'allergic to pollen and something else',
    '9a49aec5-fa1f-405d-889e-4dfc420db2b6',
    'allergicTo',
    'pollen',
    224,
    'true'
  ),
  (
    'allergic to something, but not pollen',
    '3cb8e79f-d108-4712-b620-aa146b1954a9',
    'allergicTo',
    'pollen',
    160,
    'false'
  ),
  (
    'allergic to everything',
    '1dc3fe57-7c68-4043-9d51-5457128744b2',
    'allergicTo',
    'pollen',
    255,
    'true'
  ),
  (
    'not allergic to anything',
    'd3f523d6-3d50-419b-a222-d4dfd62ce314',
    'allergicTo',
    'cats',
    0,
    'false'
  ),
  (
    'allergic only to cats',
    'eba541c3-c886-42d3-baef-c048cb7fcd8f',
    'allergicTo',
    'cats',
    128,
    'true'
  ),
  (
    'allergic to cats and something else',
    'ba718376-26e0-40b7-bbbe-060287637ea5',
    'allergicTo',
    'cats',
    192,
    'true'
  ),
  (
    'allergic to something, but not cats',
    '3c6dbf4a-5277-436f-8b88-15a206f2d6c4',
    'allergicTo',
    'cats',
    64,
    'false'
  ),
  (
    'allergic to everything',
    '1faabb05-2b98-4995-9046-d83e4a48a7c1',
    'allergicTo',
    'cats',
    255,
    'true'
  ),
  (
    'no allergies',
    'f9c1b8e7-7dc5-4887-aa93-cebdcc29dd8f',
    'list',
    '',
    0,
    ''
  ),
  (
    'just eggs',
    '9e1a4364-09a6-4d94-990f-541a94a4c1e8',
    'list',
    '',
    1,
    'eggs'
  ),
  (
    'just peanuts',
    '8851c973-805e-4283-9e01-d0c0da0e4695',
    'list',
    '',
    2,
    'peanuts'
  ),
  (
    'just strawberries',
    '2c8943cb-005e-435f-ae11-3e8fb558ea98',
    'list',
    '',
    8,
    'strawberries'
  ),
  (
    'eggs and peanuts',
    '6fa95d26-044c-48a9-8a7b-9ee46ec32c5c',
    'list',
    '',
    3,
    'eggs, peanuts'
  ),
  (
    'more than eggs but not peanuts',
    '19890e22-f63f-4c5c-a9fb-fb6eacddfe8e',
    'list',
    '',
    5,
    'eggs, shellfish'
  ),
  (
    'lots of stuff',
    '4b68f470-067c-44e4-889f-c9fe28917d2f',
    'list',
    '',
    248,
    'strawberries, tomatoes, chocolate, pollen, cats'
  ),
  (
    'everything',
    '0881b7c5-9efa-4530-91bd-68370d054bc7',
    'list',
    '',
    255,
    'eggs, peanuts, shellfish, strawberries, tomatoes, chocolate, pollen, cats'
  ),
  (
    'no allergen score parts',
    '12ce86de-b347-42a0-ab7c-2e0570f0c65b',
    'list',
    '',
    509,
    'eggs, shellfish, strawberries, tomatoes, chocolate, pollen, cats'
  ),
  (
    'no allergen score parts without highest valid score',
    '93c2df3e-4f55-4fed-8116-7513092819cd',
    'list',
    '',
    257,
    'eggs'
  );

-- Comparison of user input and the tests updates the status for each test:
UPDATE tests
SET
  status = 'pass'
FROM
  (
    SELECT
      task,
      item,
      score,
      result
    FROM
      allergies
  ) AS actual
WHERE
  (
    actual.task,
    actual.item,
    actual.score,
    actual.result
  ) = (
    tests.task,
    tests.item,
    tests.score,
    tests.expected
  );

-- Write results and debug info:
.read ./test_reporter.sql
